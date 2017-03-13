
module Chapter3.Lib where

import Prelude

import Control.Arrow ((&&&))
import Control.Lens ((.=))
import Data.Default (def)
import Graphics.Rendering.Chart.Easy
       (AxisFn, LinearAxisParams, laxis_generate, layout_x_axis,
        layout_y_axis, line, plot, scaledAxis)
import Graphics.Rendering.Chart.Gtk (toWindow)
import Numeric.LinearAlgebra (Vector, toList)

sigmoidFunction :: Double -> Double
sigmoidFunction x = 1 / (1 + exp (-x))

plotSigmoidFunction :: IO ()
plotSigmoidFunction = do
  let x = [-5.0, -4.9 .. 5.0] :: Vector Double
  toWindow 100 200 $ do
    layout_y_axis . laxis_generate .= scaleFrom (0) 1
    layout_x_axis . laxis_generate .= scaleFrom (-5) 5
    plot $ line "hello" [fmapZip sigmoidFunction $ toList x]

doScale :: AxisFn Double
doScale xs =
  scaledAxis
    (def :: LinearAxisParams Double)
    (minimum xs - 1 :: Double, maximum xs + 1 :: Double)
    xs

scaleFrom :: Double -> Double -> AxisFn Double
scaleFrom start end =
  scaledAxis (def :: LinearAxisParams Double) (start, end)

fmapZip :: Functor f => (a -> b) -> f a -> f (a, b)
fmapZip f xs = fmap (id &&& f) xs
