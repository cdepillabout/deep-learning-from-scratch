
module Chapter3.Example where

import Prelude

import Data.Monoid ((<>))
import Numeric.LinearAlgebra (Matrix, (><))

import Chapter3.Lib (sigmoidFunc)



a1 :: Matrix Double
a1 =
  let x = (1><2) [1.0, 0.5]
      w1 = (2><3) [0.1, 0.3, 0.5, 0.2, 0.4, 0.6]
      b1 = (1><3) [0.1, 0.2, 0.3]
  in (x <> w1) + b1

z1 :: Matrix Double
z1 = sigmoidFunc a1

a2 :: Matrix Double
a2 =
  let w2 = (3><2) [0.1, 0.4, 0.2, 0.5, 0.3, 0.6]
      b2 = (1><2) [0.1, 0.2]
  in (z1 <> w2) + b2

z2 :: Matrix Double
z2 = sigmoidFunc a2

a3 :: Matrix Double
a3 =
  let w3 = (2><2) [0.1, 0.3, 0.2, 0.4]
      b3 = (1><2) [0.1, 0.2]
  in (z2 <> w3) + b3

y :: Matrix Double
y = a3

