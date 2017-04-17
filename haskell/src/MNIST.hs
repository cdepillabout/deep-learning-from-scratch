
module MNIST where

import Prelude hiding (drop, dropWhile, readFile)

import Conduit
       (Consumer, Producer, (=$=), ($$), await, concatC, dropC, printC, mapC,
        sinkVector, sourceFile)
import Control.Monad (void)
import Control.Monad.Trans.Resource (MonadResource, runResourceT)
import Data.ByteString (ByteString, drop, readFile)
import Data.Word (Word8)
import Numeric.LinearAlgebra (Vector)
import Numeric.LinearAlgebra.Devel (fromByteString)
import System.FilePath ((</>))

trainImgPath :: FilePath
trainImgPath = "mnist" </> "train-images-idx3-ubyte"

trainLabelPath :: FilePath
trainLabelPath = "mnist" </> "train-labels-idx1-ubyte"

testImgPath :: FilePath
testImgPath = "mnist" </> "t10k-images-idx3-ubyte"

testLabelPath :: FilePath
testLabelPath = "mnist" </> "t10k-labels-idx1-ubyte"

loadMNIST :: IO (Vector Word8, Vector Word8, Vector Word8, Vector Word8)
loadMNIST = do
  trainImg <- loadImg trainImgPath
  trainLabel <- loadLabel trainLabelPath
  testImg <- loadImg testImgPath
  testLabel <- loadLabel testLabelPath
  pure (trainImg, trainLabel, testImg, testLabel)

loadImg :: FilePath -> IO (Vector Word8)
loadImg filePath =
  fromByteString . drop 16 <$> readFile filePath

loadLabel :: FilePath -> IO (Vector Word8)
loadLabel filePath =
  fromByteString . drop 8 <$> readFile filePath
