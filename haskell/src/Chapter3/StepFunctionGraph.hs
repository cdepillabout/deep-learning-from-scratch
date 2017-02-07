
module Chapter3.StepFunctionGraph where

import ClassyPrelude hiding (Vector, (<.>))

import Control.Lens ((.=))
import Data.Default (def)

import Graphics.Gnuplot.Simple (Attribute(..), plotFunc)

import Graphics.Rendering.Chart.Easy
       (AxisFn, LinearAxisParams, laxis_generate, layout_y_axis, line, plot, scaledAxis)
import Graphics.Rendering.Chart.Gtk (toWindow)

stepFunction :: Double -> Double
stepFunction x = if x > 0 then 1 else 0

plotStepFunctionGnu :: IO ()
plotStepFunctionGnu = do
  let x = [-5.0, -4.9 .. 5.0]
  plotFunc [YRange (-0.2, 1.2)] x stepFunction

plotStepFunctionChart :: IO ()
plotStepFunctionChart = do
  let x = [-5.0, -4.9 .. 5.0]
  toWindow 100 200 $ do
    layout_y_axis . laxis_generate .= doScale
    plot $ line "hello" [fmapZip stepFunction x]

doScale :: AxisFn Double
doScale xs =
  scaledAxis
    (def :: LinearAxisParams Double)
    (minimum (impureNonNull xs) - 1 :: Double, maximum (impureNonNull xs) + 1 :: Double)
    xs

fmapZip :: Functor f => (a -> b) -> f a -> f (a, b)
fmapZip f xs = fmap (id &&& f) xs

