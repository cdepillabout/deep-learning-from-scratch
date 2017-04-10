
module Chapter3.Lib where

import Prelude

trainImg :: FilePath
trainImg = "train-images-idx3-ubyte.gz"

trainLabel :: FilePath
trainLabel = "train-labels-idx1-ubyte.gz"

testImg :: FilePath
testImg = "t10k-images-idx3-ubyte.gz"

testLabel :: FilePath
testLabel = "t10k-labels-idx1-ubyte.gz"

data Normalize = Normalize | NoNormalize

loadMNIST :: IO (Vector Word8, Vector Word8, Vector Word8, Vector Word8)
loadMNIST = do
  -- use Conduit.conduitVector??
  -- or maybe sinkVectorN

