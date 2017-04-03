
module Chapter3.Lib where

import Prelude

import Control.Arrow ((&&&))
import Control.Lens ((.=))
import Data.Default (def)
import Graphics.Rendering.Chart.Easy
       (AxisFn, LinearAxisParams, laxis_generate, layout_x_axis,
        layout_y_axis, line, plot, scaledAxis)
import Graphics.Rendering.Chart.Gtk (toWindow)
import Numeric.LinearAlgebra
       (Container, Vector, (<#), (><), sumElements, toList)

sigmoidFunc :: Floating x => x -> x
sigmoidFunc x = 1 / (1 + exp (-x))

plotSigmoidFunc :: IO ()
plotSigmoidFunc = do
  let x = [-5.0, -4.9 .. 5.0] :: Vector Double
      y = sigmoidFunc x
  toWindow 100 200 $ do
    layout_y_axis . laxis_generate .= scaleFrom 0 1
    layout_x_axis . laxis_generate .= scaleFrom (-5) 5
    -- plot $ line "hello" [fmapZip sigmoidFunc $ toList x]
    plot $ line "hello" [zip (toList x) (toList y)]

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
fmapZip f = fmap (id &&& f)

example :: Vector Double
example = [1,2] <# (2><3) [1,3,5,2,4,6]

-- |
--
-- >>> import Numeric.LinearAlgebra (Matrix)
-- >>> let matrix = (1><3) [0.3, 2.9, 4.0] :: Matrix Double
-- >>> softmax matrix
-- (1><3)
--  [ 1.8211...e-2, 0.2451918..., 0.7365969... ]
softmax :: (Container c e, Floating (c e), Real e) => c e -> c e
softmax a =
  let expA = exp a
  in expA / realToFrac (sumElements expA)
