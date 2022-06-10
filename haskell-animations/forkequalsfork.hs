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

customDuration = 3
data Point = Point Double Double
data Segment = Segment Point Point

-- Helper function creating an Animation from a line segment
animatePart :: Segment -> Animation
animatePart (Segment (Point startx starty) (Point endx endy)) = setDuration customDuration $
  animate $ \t ->
    partialSvg t $ pathify $ mkLine (startx, starty) (endx, endy) 

-- Helper function to split a segment into smaller segments
splitSegments :: Segment -> Int -> [Segment]
splitSegments seg numSegs =
              let firstSeg = getFirstSeg seg numSegs
              in take numSegs (iterate shiftSeg firstSeg)

 -- Helper function to create the first smaller segment in a large segment
getFirstSeg :: Segment -> Int -> Segment
getFirstSeg (Segment (Point startx starty) (Point endx endy)) numSegs
             = Segment (Point startx starty) (Point ( startx + ((endx - startx)/(fromIntegral numSegs))) ( starty + ((endy - starty)/(fromIntegral numSegs))))

-- Helper function to shift a segment to the right by the size of itself
shiftSeg :: Segment -> Segment
shiftSeg (Segment (Point startx starty) (Point endx endy))
       = Segment (Point endx endy) (Point (endx + (endx-startx)) (endy + (endy-starty)))

-- Helper function to animate a fork
animateFork :: Segment -> Animation
animateFork (Segment (Point startx starty) (Point endx endy))
       = staticFrame 0.1 (mkLine (startx,starty) (endx, endy))

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
        , center $ latex txt ]

-- Helper function to label a proccess with a pid using a segment
labelPidFromSeg :: Segment -> Int -> SVG
labelPidFromSeg (Segment (Point startx starty) (Point endx endy)) i = mkGroup
            [translate (startx - 0.5) (starty) $ scale 0.2 $ createText $T.pack("pid: " ++ (show i))]
  
-- Helper function to position a line of text
mkTextLabel :: [Char] -> Point -> SVG
mkTextLabel txt (Point startx starty) = mkGroup
            [translate (startx) (starty) $ scale 0.2 $ outlineText $ T.pack(txt)]
  
-- Helper function to create a list of points in a horizontal line
mkHListofPoints :: Point -> Double -> Int -> [Point]
mkHListofPoints (Point startx starty) xoffset numPts =
          let firstPoint = (Point startx starty)
                  in take numPts (iterate (\p -> shiftPoint p (xoffset) 0) firstPoint)

-- Helper function to shift a point by a certain x and y offset value  
shiftPoint :: Point -> Double -> Double -> Point
shiftPoint (Point startx starty) xoffset yoffset = (Point (startx + (xoffset)) (starty + (yoffset)))

-- Helper function to create a label with the return value of a fork() system call
mkForkLabel :: Point -> Int -> Animation
mkForkLabel point val = oFadeIn (mkTextLabel ("fork() = " ++ (show val) ) point)                           
 
-- Helper function to speed up an animation
speedUpAnimation :: Animation -> Animation
speedUpAnimation anim = adjustDuration (*0.5) anim

-- Helper function to slow down an animation
slowAnimation :: Animation -> Animation
slowAnimation anim = adjustDuration (*1.5) anim

-- Helper function to create labels with "printf()" 
mkPrintLabels :: [Point] -> [Char] -> [Animation]
mkPrintLabels pointList txt =
           map slowAnimation ( map oFadeIn (map (\p -> mkTextLabel ("printf(" ++ (show txt) ++ ")" ) p) pointList))


main :: IO ()
main = reanimate 
       $ docEnv
       $ (parentAnimations!!0) `parA` (parentPidLabel) 
         `andThen` (forkAnimations!!0) 
         `andThen` ((firstChildPidLabel) `parA` (firstChildAnimations!!0) `parA` (parentForkLabelsAnimations !! 0) `parA` (parentAnimations!!1) `parA` (firstChildForkLabelsAnimations !! 0)) 
         `andThen` ((forkAnimations!!1) `parA` (forkAnimations!!2)) 
         `andThen`(((parentAnimations!!2) `parA` (parentForkLabelsAnimations !! 1) `parA` (secondChildAnimations!!0) `parA` (secondChildPidLabel) `parA` (secondChildForkLabelsAnimations !! 0)) `parA` ((firstChildAnimations!!1) `parA` (firstChildForkLabelsAnimations !! 1) `parA` (thirdChildAnimations!!0) `parA` (thirdChildPidLabel) `parA` (thirdChildForkLabelsAnimations !! 0)) `andThen` ((chainT parA printfLabelsNotEq) `parA` (printfLabelsEq !!0))) 
   

-- Lists of segments that make up the diagram      
parentSegments = splitSegments (Segment (Point (-6) 3 ) (Point 6 3)) 3
firstChildSegments = splitSegments (Segment (Point (-2) (-1)) (Point 6 (-1))) 2
secondChildSegments = splitSegments (Segment (Point 2 1) (Point 6 1)) 1
thirdChildSegments = splitSegments (Segment (Point 2 (-3)) (Point 6 (-3))) 1
forkSegments = [Segment (Point (-2) 3) (Point (-2) (-1)), Segment (Point 2 3) (Point 2 1), Segment (Point 2 (-1)) (Point 2 (-3))]

-- Lists of animations for the parent proccess: the line, the pid label, and the fork() system calls 
parentAnimations = map animatePart parentSegments
parentPidLabel = speedUpAnimation (oFadeIn (labelPidFromSeg (parentSegments !! 0) 1000))
parentForkLabelsAnimations = map (\(p,val) -> mkForkLabel p val) [(Point (-2) (3.25), 1001), (Point (2) (3.25), 1002)]

-- Lists of animations for the first child proccess: the line, the pid label, and the fork() system calls 
firstChildAnimations = map animatePart firstChildSegments 
firstChildPidLabel = speedUpAnimation (oFadeIn (labelPidFromSeg (firstChildSegments !! 0) 1001))
firstChildForkLabelsAnimations = map (\(p,val) -> mkForkLabel p val) [(Point (-2) (-0.75), 0), (Point (2) (-0.75), 1003)]

-- Lists of animations for the second child proccess: the line, the pid label, and the fork() system calls 
secondChildAnimations = map animatePart secondChildSegments
secondChildPidLabel = speedUpAnimation (oFadeIn (labelPidFromSeg(secondChildSegments !! 0) 1002))
secondChildForkLabelsAnimations = map (\(p,val) -> mkForkLabel p val)[(Point (2) (1.25), 0)]

-- Lists of animations for the third child proccess: the line, the pid label, and the fork() system calls 
thirdChildAnimations = map animatePart thirdChildSegments
thirdChildPidLabel = speedUpAnimation (oFadeIn (labelPidFromSeg (thirdChildSegments !! 0) 1003))
thirdChildForkLabelsAnimations = map (\(p,val) -> mkForkLabel p val) [(Point (2) (-2.75), 0)]

-- Lists of animations that label the printf() statements for each proccess
printfLabelsNotEq = mkPrintLabels [Point 6 3.25, Point 6 (-0.75), Point 6 1.25 ] "fork() == fork() is false" 
printfLabelsEq = mkPrintLabels [Point 6 (-2.75)] "fork() == fork() is true"
forkAnimations = map animateFork forkSegments

