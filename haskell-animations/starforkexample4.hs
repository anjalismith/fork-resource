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

-- Helper functions creating an Animation from two coordinates
animatePart :: Segment -> Animation
animatePart (Segment (Point startx starty) (Point endx endy)) = setDuration customDuration $
  animate $ \t ->
    partialSvg t $ pathify $ mkLine (startx, starty) (endx, endy) 

-- Helper function to create coordinates for a specific line
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

-- Helper function to animate the line segment that represents a fork
animateFork :: Segment -> Animation
animateFork (Segment (Point startx starty) (Point endx endy))
       = staticFrame 0.1 (mkLine (startx,starty) (endx, endy))

-- Helper function to label a proccess with a pid
labelPidFromPt :: Point -> Int -> SVG
labelPidFromPt (Point startx starty) i = mkGroup
            [translate (startx - 0.5) (starty) $ scale 0.2 $ createText $T.pack("pid: " ++ (show i))]

-- Helper function to position a line of text
mkTextLabel :: [Char] -> Point -> SVG
mkTextLabel txt (Point startx starty) = mkGroup
           [translate (startx) (starty) $ scale 0.15 $ outlineText $ T.pack(txt)]

-- Helper function to make a fork() label
mkForkLabel :: Int -> Point -> Animation
mkForkLabel val point = speedUpAnimation $ oFadeIn (mkTextLabel ("fork() = " ++ (show val) ) point)

-- Helper function to make an exit(0) call
mkExitLabel :: Point -> Animation
mkExitLabel pnt = speedUpAnimation $ oFadeIn (mkTextLabel "exit(0)" pnt)

-- Helper function to speed up an animation
speedUpAnimation :: Animation -> Animation
speedUpAnimation anim = adjustDuration (*0.6) anim
 
-- Helper function to slow down an animation
slowAnimation :: Animation -> Animation
slowAnimation anim = adjustDuration (*1.6) anim

-- Helper function that creates waitpid() call label
mkWaitpidLabel :: Point -> Animation
mkWaitpidLabel pnt = oFadeIn $ mkTextLabel "waitpid(pid, NULL, 0)" pnt

-- Helper function that creates star() call label
mkStarLabels :: Point -> Int -> Animation
mkStarLabels pnt val =
             oFadeIn $ mkTextLabel ("star(" ++ (show val) ++ ")" ) pnt

-- Helper function to create text label
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
       $ ((pidLabelAnims !! 0) `parA` (parentAnimations !! 0)) 
         `parA` ((speedUpAnimation (varLabelAnims !! 0)) `andThen` (speedUpAnimation (varLabelAnims !! 1)) `andThen` (starLabelAnims !! 0))
         `andThen` ((forkLabelAnims !! 0) `parA` (forkAnimations !! 0) `andThen` (forkLabelAnims !! 1)) 
         `andThen` ((waitpidLabelsAnims !! 0) `parA` (pidLabelAnims !! 1) `parA` (firstChildAnimations !! 0)  
         `parA` ((varLabelAnims !! 2) `andThen` (starLabelAnims !! 1))) 
         `andThen` ((forkLabelAnims !! 2) `parA` (forkAnimations !! 1) `andThen` (forkLabelAnims !! 3)) 
         `andThen` ((waitpidLabelsAnims !! 1) `parA` (pidLabelAnims !! 2) `parA` (secondChildAnimation) 
         `parA` (varLabelAnims !! 3)
         `andThen` ((firstChildAnimations !! 1) `parA` ((starLabelAnims !! 2) `andThen` (exitLabelAnims !! 0))))
         `andThen` ((parentAnimations !! 1) `parA` ((starLabelAnims !! 3) `andThen` (exitLabelAnims !! 1)))

--Lists of segments for the lines that represent each proccess 
parentSegments = [Segment (Point (-6) 3) (Point (0) (3)), Segment (Point 0 3) (Point (3) (3))]
firstChildSegments = splitSegments (Segment (Point (0) (0)) (Point 6 (0))) 2
secondChildSegment = Segment (Point (3) (-3)) (Point 4 (-3))
forkSegments = [Segment (Point (0) 3) (Point (0) (0)),Segment (Point 3 (0)) (Point 3 (-3))]

-- List of animations that make up the lines in the diagram
parentAnimations = map animatePart parentSegments
firstChildAnimations = map animatePart firstChildSegments
secondChildAnimation = animatePart secondChildSegment
forkAnimations = map animateFork forkSegments

-- A list of animations that label the pids of each proccess
pidLabelAnims = map oFadeIn $ map (\(p, val) -> labelPidFromPt p val) [(Point (-6) (3), 1000),(Point (0) (0), 1001), (Point (3) (-3), 1002)]

-- A list of animations that label the fork() calls at each fork
forkLabelAnims = [mkForkLabel 1001 (Point (0) 3.25), mkForkLabel 0 (Point (0) (0.25)), mkForkLabel 1002 (Point (3) (0.25)), mkForkLabel 0 (Point (3) (-2.75))]

-- A list of animations that label the variable values at specified points
varLabelAnims = map oFadeIn [mkTextLabel "n = 2" (Point (-5.5) (3.25)), mkTextLabel "i = 1" (Point (-4.5) (3.25)), mkTextLabel "i = 2" (Point (0.75) (0.25)), mkTextLabel"i = 3" (Point (3.75) (-2.75))]

-- A list of animations that label the waitpid() calls 
waitpidLabelsAnims = [mkWaitpidLabel (Point (0.75) (3.5)), mkWaitpidLabel (Point (3.75) (0.5))]

-- A list of animations that label the star() calls 
starLabelAnims = map slowAnimation [mkStarLabels (Point (-3.5) (3.25)) 1,mkStarLabels (Point (1.75) (0.25)) 2, mkStarLabels (Point (4.75) (0.25)) 2, mkStarLabels (Point (1.75) (3.25)) 1]

-- A list of animations that label exit() calls
exitLabelAnims = [mkExitLabel (Point (5.5) (0.25)), mkExitLabel (Point (2.5) (3.25))]
