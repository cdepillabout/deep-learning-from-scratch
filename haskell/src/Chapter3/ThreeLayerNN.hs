
module Chapter3.ThreeLayerNN where

import Prelude

import Data.Monoid ((<>))
import Numeric.LinearAlgebra (Matrix, (><))

import Chapter3.Lib (sigmoidFunc)


forward :: Matrix Double -> Matrix Double
forward x =
  let w1 = (2><3) [0.1, 0.3, 0.5, 0.2, 0.4, 0.6]
      b1 = (1><3) [0.1, 0.2, 0.3]
      w2 = (3><2) [0.1, 0.4, 0.2, 0.5, 0.3, 0.6]
      b2 = (1><2) [0.1, 0.2]
      w3 = (2><2) [0.1, 0.3, 0.2, 0.4]
      b3 = (1><2) [0.1, 0.2]

      a1 = (x <> w1) + b1
      z1 = sigmoidFunc a1
      a2 = (z1 <> w2) + b2
      z2 = sigmoidFunc a2
      a3 = (z2 <> w3) + b3
      y = id a3
  in y

-- |
--
-- >>> threeLayerNN
-- (1><2)
--  [ 0.316827..., 0.69627... ]
threeLayerNN :: Matrix Double
threeLayerNN =
  let x = (1><2) [1.0, 0.5]
      y = forward x
  in y
