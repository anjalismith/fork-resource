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

-- Helper function to label a proccess with a pid from a point
labelPidFromPt :: Point -> Int -> SVG
labelPidFromPt (Point startx starty) i = mkGroup
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

-- Helper function to create a list of points in a vertical line
mkVListofPoints :: Point -> Double -> Int -> [Point]
mkVListofPoints (Point startx starty) yoffset numPts =
          let firstPoint = (Point startx starty)
                  in take numPts (iterate (\p -> shiftPoint p 0 (yoffset)) firstPoint)
 
-- Helper function to shift a point by a specified x and y offset
shiftPoint :: Point -> Double -> Double -> Point
shiftPoint (Point startx starty) xoffset yoffset = (Point (startx + (xoffset)) (starty + (yoffset)))

-- Helper function to label a fork() call and its return value
mkForkLabel :: Point -> Int -> Animation
mkForkLabel point val = speedUpAnimation $ oFadeIn (mkTextLabel ("fork() = " ++ (show val) ) point)

-- Helper function to label a printf() statement
mkPrintLabelFromPnt :: [Char] -> Point -> Animation
mkPrintLabelFromPnt txt pnt =
              slowAnimation (oFadeIn ( mkTextLabel ("printf(" ++ (show txt) ++ ")" ) pnt)) 

-- Helper function to speed up an animation
speedUpAnimation :: Animation -> Animation
speedUpAnimation anim = adjustDuration (*0.3) anim 

-- Helper function to slow down an animation
slowAnimation :: Animation -> Animation
slowAnimation anim = adjustDuration (*1.5) anim
  
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
       $ ((parentAnimations!!0) `parA` (pidLabelAnims !! 0) `parA` (pid1000xanims !! 0)) `andThen` (pid1000nanims !! 0) 
         `andThen` ((forkAnimations!!0) `parA` (pid1000forkanims !! 0)) 
         `andThen` ((firstChildAnimations!!0) `parA` (pid1001forkanims !! 0) `parA`((pid1001xanims !! 0) `andThen` (pid1001printfanims !! 0))
         `parA` (parentAnimations!!1) `parA` (pidLabelAnims !! 1)) 
         `andThen` ((pid1000nanims !! 1) `parA` (pid1001nanims !! 0))
         `andThen` ((forkAnimations!!1) `parA` (pid1001forkanims !! 1) `parA` (forkAnimations!!2) `parA` (pid1000forkanims !! 1) `parA` (pid1002forkanims !! 0) `parA` (pid1003forkanims !! 0)) 
         `andThen`( ((parentAnimations!!2) `parA` (pidLabelAnims !! 2) 
         `parA` (secondChildAnimations!!0) `parA` ((pid1002xanims !! 0) `andThen` (pid1002printfanims !! 0))) 
         `parA` ((firstChildAnimations!!1) `parA` (pidLabelAnims !! 3) 
         `parA` (thirdChildAnimations!!0) `parA` ((pid1003xanims !! 0) `andThen` (pid1003printfanims !! 0)))) 
         `andThen` ((pid1000nanims !! 2) `parA` (pid1001nanims !! 1) `parA` (pid1002nanims !! 0) `parA` (pid1003nanims !! 0))
         `andThen` ((forkAnimations!!3) `parA` (pid1000forkanims !! 0)
         `parA` (forkAnimations!!4) `parA` (pid1001forkanims !! 2) 
         `parA` (forkAnimations!!5) `parA` (pid1002forkanims !! 1) `parA` (pid1003forkanims !! 1)
         `parA` (forkAnimations!!6) `parA` (chainT parA thirdforklabelanims))
         `andThen` (((pidLabelAnims !! 4) `parA` (parentAnimations!!3) `parA` (fourthChildAnimations!!0)) 
         `parA` ((pidLabelAnims !! 5) `parA` (secondChildAnimations!!1) `parA` (fifthChildAnimations!!0))
         `parA` ((pidLabelAnims !! 6) `parA` (firstChildAnimations!!2) `parA` (sixthChildAnimations!!0)) 
         `parA` ((pidLabelAnims !! 7) `parA` (thirdChildAnimations!!1) `parA` (seventhChildAnimations!!0)) 
         `parA` ((chainT parA thirdxlabelanims) `andThen` (chainT parA thirdprintfanims))) 
         `andThen` (chainT parA thirdnlabelanims)

-- Segments that make up the lines of the diagram 
parentSegments = splitSegments (Segment (Point (-6) 3 ) (Point 6 3)) 4
firstChildSegments = splitSegments (Segment (Point (-3) (-1)) (Point 6 (-1))) 3
secondChildSegments = splitSegments (Segment (Point 0 1) (Point 6 1)) 2
thirdChildSegments = splitSegments (Segment (Point 0 (-3)) (Point 6 (-3))) 2
fourthChildSegments = splitSegments (Segment (Point 3 2) (Point 6 2)) 1
fifthChildSegments = splitSegments (Segment (Point 3 0) (Point 6 0)) 1
sixthChildSegments = splitSegments (Segment (Point 3 (-2)) (Point 6 (-2))) 1
seventhChildSegments = splitSegments (Segment (Point 3 (-4)) (Point 6 (-4))) 1
forkSegments = [Segment (Point (-3) 3) (Point (-3) (-1)), Segment (Point 0 3) (Point 0 1), Segment (Point 0 (-1)) (Point 0 (-3)), Segment (Point 3 3) (Point 3 2), Segment (Point 3 1) (Point 3 0), Segment (Point 3 (-1)) (Point 3 (-2)), Segment (Point 3 (-3)) (Point 3 (-4))]

-- Lists of animations for each line segment
parentAnimations = map animatePart parentSegments
firstChildAnimations = map animatePart firstChildSegments 
secondChildAnimations = map animatePart secondChildSegments
thirdChildAnimations = map animatePart thirdChildSegments
fourthChildAnimations = map animatePart fourthChildSegments
fifthChildAnimations = map animatePart fifthChildSegments
sixthChildAnimations = map animatePart sixthChildSegments
seventhChildAnimations = map animatePart seventhChildSegments
forkAnimations = map animateFork forkSegments

-- List of animations that label the pid of each proccess
pidLabelAnims = map oFadeIn $ map (\(p, val) -> labelPidFromPt p val) [(Point (-6) (3), 1000),(Point (-3) (-1), 1001), (Point (0) (1), 1002), (Point (0) (-3), 1003), (Point (3) (2), 1004), (Point (3) (0), 1005), (Point (3) (-2), 1006), (Point (3) (-4), 1007)]

-- Lists of animations that label the fork() calls and their return values
pid1000forkanims = [mkForkLabel (Point (-3) 3.25) 1001, mkForkLabel (Point (0) 3.25) 1002]
pid1001forkanims = [mkForkLabel (Point (-3) (-0.75)) 0, mkForkLabel (Point (0) (-0.75)) 1003, mkForkLabel (Point (3) (-0.75)) 1006]
pid1002forkanims = [mkForkLabel (Point (0) (1.25)) 0, mkForkLabel (Point (3) (1.25)) 1005]
pid1003forkanims = [mkForkLabel (Point (0) (-2.75)) 0, mkForkLabel (Point (3) (-2.75)) 1007]
thirdforklabelanims = [mkForkLabel (Point (3) (2.25)) 0, mkForkLabel (Point (3) (0.25)) 0, mkForkLabel (Point (3) (-1.75)) 0, mkForkLabel (Point (3) (-3.75)) 0, mkForkLabel (Point (3) 3.25) 1004]

-- Lists of animations that label the value of the variable n
pid1000nanims = map oFadeIn [mkTextLabel "n = 3" (Point (-3.4) 3.5), mkTextLabel "n=2" (Point (-0.4) 3.5), mkTextLabel "n=1" (Point (2.6) 3.5)]
pid1001nanims = map oFadeIn [mkTextLabel "n=2" (Point (-0.4) (-0.5)), mkTextLabel "n=1" (Point (2.6) (-0.5))]
pid1002nanims = map oFadeIn [mkTextLabel "n=1" (Point (2.6) (1.5))]
pid1003nanims = map oFadeIn [mkTextLabel "n=1" (Point (2.6) (-2.5))]
thirdnlabelanims = map oFadeIn [mkTextLabel  "n=0" (Point (5.8) (3.25)),mkTextLabel "n=0" (Point (5.8) (2.25)), mkTextLabel  "n=0" (Point (5.8) (1.25)), mkTextLabel "n=0" (Point (5.8) (0.25)), mkTextLabel "n=0" (Point (5.8) (-0.75)), mkTextLabel "n=0" (Point (5.8) (-1.75)), mkTextLabel "n=0" (Point (5.8) (-2.75)), mkTextLabel "n=0" (Point (5.8) (-3.75))]

-- Lists of animations that label the value of the variable x
pid1000xanims = map oFadeIn [mkTextLabel "x = 1" (Point (-5.75) 3.25) ]
pid1001xanims = map oFadeIn [mkTextLabel "x *= 2" (Point (-2.25) (-0.75))]
pid1002xanims = map oFadeIn [mkTextLabel "x *= 2" (Point (0.75) (1.25))]
pid1003xanims = map oFadeIn [mkTextLabel "x *= 2" (Point (0.75) (-2.75))]
thirdxlabelanims = map oFadeIn [mkTextLabel "x *= 2" (Point (3.75) (0.25)), mkTextLabel "x *= 2" (Point (3.75) (-1.75)), mkTextLabel "x *= 2" (Point (3.75) (-3.75)), mkTextLabel "x *= 2" (Point (3.75) 2.25)]

-- Lists of animations that label the printf() statements
pid1001printfanims = map slowAnimation [mkPrintLabelFromPnt "2" (Point (-1.5) (-0.75))]
pid1002printfanims = map slowAnimation [mkPrintLabelFromPnt "2" (Point (1.5) (1.25))]
pid1003printfanims = map slowAnimation [mkPrintLabelFromPnt "4" (Point (1.5) (-2.75))]
thirdprintfanims = map slowAnimation [mkPrintLabelFromPnt "2" (Point (4.5) 2.25), mkPrintLabelFromPnt "4" (Point (4.5) (0.25)), mkPrintLabelFromPnt "4" (Point (4.5) (-1.75)), mkPrintLabelFromPnt "8" (Point (4.5) (-3.75))]

