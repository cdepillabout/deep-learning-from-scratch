
module Chapter2.Perceptron where

import ClassyPrelude hiding (Vector, (<.>))

import Numeric.LinearAlgebra (R, Vector, (<.>))

andPerceptron :: Vector R -> Double
andPerceptron inputVect =
  let w = [0.5, 0.5]
      b = -0.7
      dotProd = inputVect <.> w
      tmp = dotProd + b
  in if tmp <= 0 then 0 else 1

nandPerceptron :: Vector R -> Double
nandPerceptron inputVect =
  let w = [-0.5, -0.5]
      b = 0.7
      dotProd = inputVect <.> w
      tmp = dotProd + b
  in if tmp <= 0 then 0 else 1

orPerceptron :: Vector R -> Double
orPerceptron inputVect =
  let w = [0.5, 0.5]
      b = -0.2
      dotProd = inputVect <.> w
      tmp = dotProd + b
  in if tmp <= 0 then 0 else 1

xorPerceptron :: Vector R -> Double
xorPerceptron inputVect =
  let s1 = nandPerceptron inputVect
      s2 = orPerceptron inputVect
  in andPerceptron [s1, s2]
