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
import Control.Lens
import Linear.V2

customDuration = 3
data Point = Point Double Double
data Segment = Segment Point Point

-- Helper function creating an Animation from a line segment
animatePart :: Segment -> Animation
animatePart (Segment (Point startx starty) (Point endx endy)) = setDuration customDuration $
  animate $ \t ->
    partialSvg t $ pathify $ mkLine (startx, starty) (endx, endy) 

-- Helper function creating a sleep Animation from a line segment
animateSleepFromSegment :: Segment -> Animation
animateSleepFromSegment (Segment (Point startx starty) (Point endx endy))  = setDuration (endx - startx) $
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

-- Helper function to create a copy of a segment at a specified location 
makeCopyofSeg :: Segment -> Double -> Double -> Segment
makeCopyofSeg (Segment (Point startx starty) (Point endx endy)) offsetx offsety
       = Segment (Point (startx + offsetx) (starty + (offsety))) (Point (endx + (offsetx)) (endy + (offsety)))

-- Helper function to animate a fork
animateFork :: Segment -> Animation
animateFork (Segment (Point startx starty) (Point endx endy))
       = staticFrame 0.4 (mkLine (startx,starty) (endx, endy))

--Helper function to label a proccess with a pid
labelPidFromPt :: Point -> Int -> SVG
labelPidFromPt (Point startx starty) i = mkGroup
           [translate (startx - 1) (starty) $ scale 0.2 $ createText $T.pack("pid: " ++ (show i))]

--Helper function to position a line of text
mkTextLabel:: [Char] -> Point -> SVG
mkTextLabel txt (Point startx starty) = mkGroup
          [translate (startx) (starty) $ scale 0.15 $ outlineText $ T.pack(txt)]

-- Helper function to label a fork() call and its return value
mkForkLabel :: Point -> Int -> Animation
mkForkLabel point val = speedUpAnimation $ oFadeIn (mkTextLabel("fork() = " ++ (show val) ) point)
 
--Helper function to speed up an animation
speedUpAnimation :: Animation -> Animation
speedUpAnimation anim = adjustDuration (*0.3) anim
 
 
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
-- Helper function that labels printf() calls using a list of points
mkPrintLabels :: [Point] -> [Char] -> [Animation]
mkPrintLabels pointList txt =
              map slowAnimation ( map oFadeIn (map (\p -> mkTextLabel ("printf(" ++ (show txt) ++ ")" ) p) pointList))

-- Helper function that labels a printf() call at a specified point
mkPrintLabelFromPnt :: Point -> [Char] -> Animation
mkPrintLabelFromPnt pnt txt =
              slowAnimation (oFadeIn ( mkTextLabel ("printf(" ++ (show txt) ++ ")" ) pnt))

-- Helper function to create a countdown at a specified point
displayCounter :: Point -> Double -> [Animation]
displayCounter pnt counter = 
             (map oFadeIn (map (\val -> mkTextLabel (show val) pnt) (reverse [0..counter])))

-- Helper function to display sleep calls
mkSleepLabel :: Point -> Animation
mkSleepLabel pnt = 
             (oFadeIn ( mkTextLabel ("sleep:") pnt))

-- Helper function to make an exit(0) call
mkExitLabel :: Point -> Animation
mkExitLabel pnt = speedUpAnimation $ oFadeIn (mkTextLabel "exit(0)" pnt)

main :: IO ()
main = reanimate 
       $ docEnv
       $ ((pidLabelAnimations !! 0) `parA` (startSegmentsAnims !! 0))
         `andThen` (pid1000forklabel `parA` (forkSegmentsAnims !! 0) `parA` (pid1001forklabelchild))
         `andThen` ((sleep3Anim `parA` (sleepSegmentsAnims!! 0) `andThen` printf3Anim `andThen` (exitLabelAnims !! 0)) 
         `parA` ((pidLabelAnimations !! 1) `parA` (speedUpAnimation (startSegmentsAnims !! 1)) 
         `andThen` (pid1001forklabelparent `parA` (forkSegmentsAnims !! 1) `parA` pid1002forklabelchild)
         `andThen` ((sleep1Anim `parA` (sleepSegmentsAnims !! 1) `andThen` printf1Anim `andThen` (exitLabelAnims !! 1))
         `parA`(((pidLabelAnimations !! 2) `parA` (startSegmentsAnims !! 2) 
         `andThen` ((forkSegmentsAnims !! 2)) 
         `andThen` (pid1002forklabelparent `parA` (sleep5Anim `parA` (sleepSegmentsAnims !! 2) `andThen` printf5Anim `andThen` (exitLabelAnims !! 2)) 
         `parA` ((pidLabelAnimations !! 3) `parA` (startSegmentsAnims !! 3) 
         `andThen` ((forkSegmentsAnims !! 3) `parA` pid1003forklabelchild `andThen` pid1003forklabelparent
         `andThen` ((sleep7Anim `parA` (sleepSegmentsAnims !! 3) `andThen` printf7Anim `andThen` (exitLabelAnims !! 3))
         `parA` (pidLabelAnimations !! 4) `parA` (startSegmentsAnims !! 4) `parA` pid1004forklabel)))))))))

--List of segments for the start of the animation
startSegments = [Segment (Point (-5) 3) (Point (-4) 3), Segment (Point (-4) 2) (Point (-3) 2), Segment (Point (-3) (1)) (Point (-2) (1)),Segment (Point (-2) (0)) (Point (-1) (0)), Segment (Point (-1) (-1)) (Point 0 (-1))]

--List of animations for the start segments
startSegmentsAnims = map animatePart startSegments

--List of segments that make up the sleeping proccesses
sleepSegments = [Segment (Point (-4) (3) ) (Point (-1) (3) ), Segment (Point (-3) (2) ) (Point (-2) (2)), Segment (Point (-2) (1) ) (Point (3) (1)), Segment (Point (-1) (0)) (Point (6) (0) )]

--List of animations for the sleep segments
sleepSegmentsAnims = map animateSleepFromSegment sleepSegments

--List of segments that make up the fork lines
forkSegments = [Segment (Point (-4) (3)) (Point (-4) (2)), Segment (Point (-3) (2)) (Point (-3) (1)), Segment (Point (-2) (1)) (Point (-2) (0)), Segment (Point (-1) (0)) (Point (-1) (-1))]

--List of animations for the fork segments
forkSegmentsAnims = map animateFork forkSegments 

--List of animations to label each proccess with its PID
pidLabelAnimations = [speedUpAnimation (oFadeIn (labelPidFromPt (Point (-5) 3 ) 1000)), speedUpAnimation (oFadeIn (labelPidFromPt (Point (-5) 2) 1001)), speedUpAnimation (oFadeIn (labelPidFromPt(Point (-5) (1)) 1002)), speedUpAnimation (oFadeIn (labelPidFromPt (Point (-5) (0)) 1003)), speedUpAnimation (oFadeIn (labelPidFromPt (Point (-5) (-1)) 1004))]

--Animations for fork() labels
pid1000forklabel = mkForkLabel (Point (-4) 3.25) 1001
pid1001forklabelchild = mkForkLabel (Point (-4) 2.25) 0
pid1001forklabelparent = mkForkLabel (Point (-3) 2.25) 1002
pid1002forklabelchild = mkForkLabel (Point (-3) 1.25) 0
pid1002forklabelparent = mkForkLabel (Point (-2) 1.25) 1003
pid1003forklabelchild = mkForkLabel (Point (-2) 0.25) 0
pid1003forklabelparent = mkForkLabel (Point (-1) 0.25) 1004
pid1004forklabel = mkForkLabel (Point (-1) (-1.25)) 0

--Animations that label printf() statements
printf3Anim = mkPrintLabelFromPnt (Point (-1) (3.25)) "3"
printf1Anim = mkPrintLabelFromPnt (Point (-2) (2.25)) "1"
printf5Anim = mkPrintLabelFromPnt (Point (3) (1.25))"5"
printf7Anim = mkPrintLabelFromPnt (Point (6) (0.25)) "7"

--Animations that add labels for sleep calls
sleep3Anim = mkSleepLabel (Point (6.25) (3) ) `parA` (chainT andThen (displayCounter (Point (6.65) (3) ) 3))
sleep1Anim = mkSleepLabel (Point (6.25) (2) ) `parA` (chainT andThen (displayCounter (Point (6.65) (2) ) 1))
sleep5Anim = mkSleepLabel (Point (6.25) (1) ) `parA` (chainT andThen (displayCounter (Point (6.65) (1) ) 5))
sleep7Anim = mkSleepLabel (Point (6.25) (0) ) `parA` (chainT andThen (displayCounter (Point (6.65) (0) ) 7))

-- Animation that labels exit(0) calls
exitLabelAnims = [mkExitLabel (Point (-0.5) (3.5)), mkExitLabel (Point (-1.5) (2.5)), mkExitLabel (Point (3.5) (1.5)), mkExitLabel (Point (6.5) (0.5))]
