
module Chapter3.StepFunctionGraph where

import ClassyPrelude hiding (Vector, (<.>))

import Graphics.Gnuplot.Simple (Attribute(..), plotFunc)

import Graphics.Rendering.Chart.Easy (line, plot)
import Graphics.Rendering.Chart.Gtk (toWindow)

stepFunction :: Double -> Double
stepFunction x = if x > 0 then 1 else 0

plotStepFunctionGnu :: IO ()
plotStepFunctionGnu = do
  let x = [-5.0, -4.9 .. 5.0]
  plotFunc [YRange (-0.2, 1.2)] x stepFunction

plotStepFunctionChart :: IO ()
plotStepFunctionChart = do
  -- TODO: write this function
  toWindow 100 200 . plot $ line "hello" [[(1 :: Int,2 :: Int), (2,10), (3, 4)]]

