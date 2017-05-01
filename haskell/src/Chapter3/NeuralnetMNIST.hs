
module Chapter3.NeuralnetMNIST where

import Prelude hiding (sum)

import Data.Monoid ((<>))
import Data.Traversable (forM)
import Data.Word (Word8)
import Data.Yaml (decodeFile)
import Numeric.LinearAlgebra
       (Matrix, Vector, asRow, fromLists, maxIndex, toList, toRows)
import System.FilePath ((</>))

import Chapter3.Lib (sigmoidFunc, softmax)
import MNIST (MNIST, MNISTContainer(..), loadNormalizedMNIST)

data Ws' i = Ws i i i deriving Show
type Ws = Ws' (Matrix Double)

data Bs' i = Bs i i i deriving Show
type Bs = Bs' (Matrix Double)

w1Path, w2Path, w3Path :: FilePath
w1Path = "mnist" </> "W1.yml"
w2Path = "mnist" </> "W2.yml"
w3Path = "mnist" </> "W3.yml"

b1Path, b2Path, b3Path :: FilePath
b1Path = "mnist" </> "b1.yml"
b2Path = "mnist" </> "b2.yml"
b3Path = "mnist" </> "b3.yml"

loadW :: FilePath -> IO (Matrix Double)
loadW filePath = do
  maybeDoubles <- decodeFile filePath
  case maybeDoubles of
    Nothing -> fail $ "Failed to load " <> filePath
    Just doubles -> pure $ fromLists doubles

loadB :: FilePath -> IO (Matrix Double)
loadB filePath = do
  maybeDoubles <- decodeFile filePath
  case maybeDoubles of
    Nothing -> fail $ "Failed to load " <> filePath
    Just doubles -> pure $ fromLists [doubles]

loadWs :: IO Ws
loadWs = Ws <$> loadW w1Path <*> loadW w2Path <*> loadW w3Path

loadBs :: IO Bs
loadBs = Bs <$> loadB b1Path <*> loadB b2Path <*> loadB b3Path

load :: IO (MNIST, Ws, Bs)
load = (,,) <$> loadNormalizedMNIST <*> loadWs <*> loadBs

predict :: Ws -> Bs -> Vector Double -> Matrix Double
predict (Ws w1 w2 w3) (Bs b1 b2 b3) x =
  let x' = asRow x
      a1 = (x' <> w1) + b1
      z1 = sigmoidFunc a1
      a2 = (z1 <> w2) + b2
      z2 = sigmoidFunc a2
      a3 = (z2 <> w3) + b3
      y = softmax a3
  in y

runNeuralnetMNIST :: IO ()
runNeuralnetMNIST = do
  (MNIST{testImg, testLabel}, ws, bs) <- load
  let testList = createTest testImg testLabel
  res <- forM testList $ f ws bs
  let (sum, total) = process res
  putStrLn $ "accuracy: " <> show ((fromIntegral sum :: Double) / (fromIntegral total :: Double))
  where
    createTest :: Matrix Double -> Vector Word8 -> [((Vector Double, Word8), Integer)]
    createTest img label = zip (zip (toRows img) (toList label)) [0..]

    f :: Ws -> Bs -> ((Vector Double, Word8), Integer) -> IO (Int, Int, Bool)
    f ws bs ((img, label), i) = do
      let y = predict ws bs img
          (_, maxIdx) = maxIndex y
          lbl = fromIntegral label
      -- putStrLn $ show i <> ": maxIdx = " <> show maxIdx <> ", lbl = " <> show lbl
      pure (maxIdx, lbl, maxIdx == lbl)

    process :: [(a, b, Bool)] -> (Int, Int)
    process = foldl g (0, 0)

    g :: (Int, Int) -> (a, b, Bool) -> (Int, Int)
    g (sum, total) (_, _, True) = (sum + 1, total + 1)
    g (sum, total) (_, _, False) = (sum, total + 1)
