#!/usr/bin/env stack
-- stack runghc --package reanimate
module Main (main) where

import           Reanimate
import           Reanimate.Builtin.Documentation
import           Reanimate.Transition
import           Reanimate.Svg
import           Reanimate.Scene
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Graphics.SvgTree                hiding (Point, Text, height)
import           Text.Printf

customDuration = 5
data Point = Point Double Double
data Segment = Segment Point Point

-- Helper functions creating an Animation from two coordinates
animatePart :: Segment -> Animation
animatePart (Segment (Point startx starty) (Point endx endy)) = setDuration customDuration $
  animate $ \t ->
    partialSvg t $ pathify $ mkLine (startx, starty) (endx, endy) 

-- Helper function creating a sleep Animation from a line segment
splitSegments :: Segment -> Int -> [Segment]
splitSegments seg numSegs =
              let firstSeg = getFirstSeg seg numSegs
              in take numSegs (iterate shiftSeg firstSeg)

-- Helper function to create the first smaller segment of a line
getFirstSeg :: Segment -> Int -> Segment
getFirstSeg (Segment (Point startx starty) (Point endx endy)) numSegs
             = Segment (Point startx starty) (Point ( startx + ((endx - startx)/(fromIntegral numSegs))) ( starty + ((endy - starty)/(fromIntegral numSegs))))

-- Helper function to shift a segment around the endpoint of the first segment
shiftSeg :: Segment -> Segment
shiftSeg (Segment (Point startx starty) (Point endx endy))
       = Segment (Point endx endy) (Point (endx + (endx-startx)) (endy + (endy-starty)))

-- Helper function to animate the line that represents a fork
animateFork :: Segment -> Animation
animateFork (Segment (Point startx starty) (Point endx endy))
       = staticFrame 0.1 (mkLine (startx,starty) (endx, endy))

-- Helper function to label a proccess with a pid
labelPids :: Segment -> Int -> SVG
labelPids (Segment (Point startx starty) (Point endx endy)) i = mkGroup 
          [translate (startx - 0.5) (starty) $ scale 0.2 $ createText $T.pack("pid: " ++ (show i))]

-- Helper function to position a line of text
mkTextLabel :: [Char] -> Point -> SVG
mkTextLabel txt (Point startx starty) = mkGroup 
          [translate (startx) (starty) $ scale 0.15 $ outlineText $ T.pack(txt)]

-- Helper function to create a list of points in a horizontal line
mkHListofPoints :: Point -> Double -> Int -> [Point]
mkHListofPoints (Point startx starty) xoffset numPts = 
        let firstPoint = (Point startx starty)
                in take numPts (iterate (\p -> shiftPoint p (xoffset) 0) firstPoint)

-- Helper function to shift a point by specified x and y offset values
shiftPoint :: Point -> Double -> Double -> Point
shiftPoint (Point startx starty) xoffset yoffset = (Point (startx + (xoffset)) (starty + (yoffset)))

-- Helper function to create list of animations that run alongside the proccesses
mkVarLabels :: [Point] -> [Animation]
mkVarLabels pointList  = 
         map slowAnimation ( map oFadeIn (map (\(i,p) -> mkTextLabel ("i = " ++ (show i)) p) (zip [0.. (length pointList)] pointList)))

-- Helper function that labels printf() calls using a list of points
mkPrintLabels :: [Point] -> [Char] -> [Animation]
mkPrintLabels pointList txt = 
         map slowAnimation ( map oFadeIn (map (\p -> mkTextLabel ("printf(" ++ (show txt) ++ ")" ) p) pointList))

-- Helper function to label a fork() call and its return value
mkForkLabel :: Point -> Int -> Animation
mkForkLabel point val = oFadeIn (mkTextLabel ("fork() = " ++ (show val) ) point) 

--Helper function to speed up an animation 
speedUpAnimation :: Animation -> Animation
speedUpAnimation anim = adjustDuration (*0.5) anim

--Helper function to slow down an animation 
slowAnimation :: Animation -> Animation
slowAnimation anim = adjustDuration (*1.5) anim

--Helper function to create text label
createText :: Text -> SVG
createText txt = mkGroup
            [ center
            $ withStrokeColorPixel rtfdBackgroundColor
            $ withStrokeWidth (0)
            $ withFillOpacity 0
            $ latex txt
            , center $ latex txt ]

-- Helper function that creates an SVG of outlined text
outlineText :: Text -> SVG
outlineText txt = mkGroup
        [ center
        $ withStrokeColorPixel rtfdBackgroundColor
        $ withStrokeWidth (defaultStrokeWidth * 8)
        $ withFillOpacity 0
        $ latex txt
        , center $ latex txt
        ]


main :: IO ()
main = reanimate 
       $ docEnv
       $ ((pidLabelAnimations !! 0) `parA` (parentAnimationBeforeFork !! 0))
         `andThen` ((forkAnimations !! 0) `parA` (parentForkLabelsAnimation) `parA` (childForkLabelsAnimation))
         `andThen` ((parentAnimationAfterFork !! 0) 
         `parA` ((pidLabelAnimations !! 1) `parA` (childAnimations !! 0))
         `parA` ((chainT andThen varLabelsPAnimations) `parA` (chainT andThen varLabelsCAnimations)
         `parA` (chainT andThen parentPrintLabelsAnimations) `parA` (chainT andThen childPrintLabelsAnimations))) 
         

-- Lists of segments that make up the lines in the diagram
parentSegmentBeforeFork = splitSegments (Segment (Point (-5) 0) (Point (-4) 0)) 1 
parentSegmentAfterFork = splitSegments (Segment (Point (-4) 0) (Point 5 0)) 1
childSegments = splitSegments (Segment (Point (-4) (-3)) (Point 5 (-3))) 1
forkSegments = [Segment (Point (-4) 0 ) (Point (-4) (-3))]

-- Lists of animations of the above line segments
parentAnimationBeforeFork = map animatePart parentSegmentBeforeFork
parentAnimationAfterFork = map animatePart parentSegmentAfterFork 
childAnimations = map animatePart childSegments
forkAnimations = map animateFork forkSegments

-- List of animations that label the pid of each proccess
pidLabelAnimations = [speedUpAnimation (oFadeIn (labelPids (parentSegmentBeforeFork !! 0) 1000)), speedUpAnimation (oFadeIn (labelPids (childSegments !! 0) 1001))]

-- Lists of animations that label the changing values of the variable i across the proccess
varLabelsPAnimations = mkVarLabels (mkHListofPoints (Point (-3) 0.5) 2 4)
varLabelsCAnimations = mkVarLabels (mkHListofPoints (Point (-3) (-3.25)) 2 4)

-- Lists of animations that label the printf() calls
printLabelsPAnimations = mkPrintLabels ((mkHListofPoints (Point (-2.25) 0.25) 2 3)) "Listen to me"
printLabelsCAnimations = mkPrintLabels (mkHListofPoints (Point (-2.25) (-3.5)) 2 3) "No way"
parentPrintLabelsAnimations = printLabelsPAnimations ++ (mkPrintLabels [(Point 3.75 0.25)] "I give up...")
childPrintLabelsAnimations = printLabelsCAnimations ++ (mkPrintLabels [(Point 3.75 (-3.5))] "Whatever")

-- Lists of animations that label the fork() calls and their return values
parentForkLabelsAnimation = mkForkLabel (Point (-4) 0.25) 1001 
childForkLabelsAnimation = mkForkLabel (Point (-4) (-2.75)) 0 

