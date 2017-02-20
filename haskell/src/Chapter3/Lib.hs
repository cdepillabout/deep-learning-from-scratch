
module Chapter3.Lib where

import Prelude

import Control.Arrow ((&&&))
import Control.Lens ((.=))
import Data.Default (def)
import Graphics.Rendering.Chart.Easy
       (AxisFn, LinearAxisParams, laxis_generate, layout_y_axis, line, plot, scaledAxis)
import Graphics.Rendering.Chart.Gtk (toWindow)

sigmoidFunction :: Double -> Double
sigmoidFunction x = 1 / (1 + exp (-x))

plotSigmoidFunction :: IO ()
plotSigmoidFunction = do
  let x = [-5.0, -4.9 .. 5.0]
  toWindow 100 200 $ do
    layout_y_axis . laxis_generate .= doScale
    plot $ line "hello" [fmapZip sigmoidFunction x]

doScale :: AxisFn Double
doScale xs =
  scaledAxis
    (def :: LinearAxisParams Double)
    (minimum xs - 1 :: Double, maximum xs + 1 :: Double)
    xs

doScaleFrom :: Double -> Double -> AxisFn Double
doScaleFrom start end =
  scaledAxis (def :: LinearAxisParams Double) (start, end)

fmapZip :: Functor f => (a -> b) -> f a -> f (a, b)
fmapZip f xs = fmap (id &&& f) xs
