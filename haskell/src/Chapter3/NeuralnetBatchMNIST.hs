
module Chapter3.NeuralnetBatchMNIST where

import Prelude hiding (sum)

import Data.Monoid ((<>))
import Data.Traversable (forM)
import Data.Word (Word8)
import Numeric.LinearAlgebra
       (Matrix, Vector, maxIndex, subMatrix, subVector, toList, toRows)

import Chapter3.Lib (sigmoidFunc, softmax)
import Chapter3.NeuralnetMNIST (Bs'(Bs), Bs, Ws'(Ws), Ws, load)
import MNIST (MNISTContainer(..))

predict :: Ws -> Bs -> Matrix Double -> Matrix Double
predict (Ws w1 w2 w3) (Bs b1 b2 b3) xs =
  let a1 = (xs <> w1) + b1
      z1 = sigmoidFunc a1
      a2 = (z1 <> w2) + b2
      z2 = sigmoidFunc a2
      a3 = (z2 <> w3) + b3
      ys = softmax a3
  in ys

runNeuralnetBatchMNIST :: IO ()
runNeuralnetBatchMNIST = do
  (MNIST{testImg, testLabel}, ws, bs) <- load
  let testList = createTest testImg testLabel
  res <- forM testList $ f ws bs
  let (sum, total) = process res
  putStrLn $
    "accuracy: " <> show ((fromIntegral sum :: Double) / (fromIntegral total :: Double))
  where
    createTest
      :: Matrix Double -> Vector Word8 -> [((Matrix Double, Vector Word8), Int)]
    createTest img label = zip (zip (batchImg img) (batchLabel label)) indicies

    f :: Ws -> Bs -> ((Matrix Double, Vector Word8), Int) -> IO ([Int], [Int], [Bool])
    f ws bs ((imgs, labels), i) = do
      let ys = predict ws bs imgs :: Matrix Double
          maxIdxes = fmap maxIndex $ toRows ys :: [Int]
          lbls = fmap fromIntegral $ toList labels :: [Int]
      -- putStrLn $
      --   "\n\n" <>
      --   show i <>
      --   ":\n\tmaxIdx = " <>
      --   show maxIdxes <>
      --   "\n\tlbl = " <>
      --   show lbls
      pure (maxIdxes, lbls, zipWith (==) maxIdxes lbls)

    process :: [(a, b, [Bool])] -> (Int, Int)
    process = foldl g (0, 0)

    g :: (Int, Int) -> (a, b, [Bool]) -> (Int, Int)
    g (sum, total) (_, _, boolList) =
      let (s, t) = h boolList
      in (sum + s, total + t)

    h :: [Bool] -> (Int, Int)
    h = foldl h' (0, 0)

    h' :: (Int, Int) -> Bool -> (Int, Int)
    h' (s, t) True = (s + 1, t + 1)
    h' (s, t) False = (s, t + 1)

batchLabel :: Vector Word8 -> [Vector Word8]
batchLabel label =
  foldr
    (\idx vectors -> subVector idx batchSize label : vectors)
    []
    indicies

batchImg :: Matrix Double -> [Matrix Double]
batchImg img =
  foldr
    (\idx matricies -> subMatrix (idx, 0) (batchSize, imgColumnSize) img : matricies)
    []
    indicies

indicies :: (Enum a, Num a) => [a]
indicies = [0, batchSize .. (10000 - batchSize)]

batchSize :: Num a => a
batchSize = 100

imgColumnSize :: Num a => a
imgColumnSize = 784

