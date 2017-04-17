
module MNIST where

import Prelude hiding (drop, dropWhile, map, readFile)

import Data.ByteString (drop, readFile)
import Data.Vector.Storable (map)
import Data.Word (Word8)
import Numeric.LinearAlgebra (Matrix, Vector, cmap, reshape)
import Numeric.LinearAlgebra.Devel (fromByteString)
import System.FilePath ((</>))

imgSize :: Num a => a
imgSize = 784

trainImgPath :: FilePath
trainImgPath = "mnist" </> "train-images-idx3-ubyte"

trainLabelPath :: FilePath
trainLabelPath = "mnist" </> "train-labels-idx1-ubyte"

testImgPath :: FilePath
testImgPath = "mnist" </> "t10k-images-idx3-ubyte"

testLabelPath :: FilePath
testLabelPath = "mnist" </> "t10k-labels-idx1-ubyte"

---------------------
-- MNIST Data Type --
---------------------

data MNIST a = MNIST
  { trainImg :: !a
  , trainLabel :: !(Vector Word8)
  , testImg :: !a
  , testLabel :: !(Vector Word8)
  }

instance Functor MNIST where
  fmap :: (a -> b) -> MNIST a -> MNIST b
  fmap f mnist =
    MNIST
    { trainImg = f $ trainImg mnist
    , trainLabel = trainLabel mnist
    , testImg = f $ testImg mnist
    , testLabel = testLabel mnist
    }

type RawMNIST = MNIST (Matrix Word8)

type NormalMNIST = MNIST (Matrix Double)

normalize :: RawMNIST -> NormalMNIST
normalize = fmap (undefined {- cmap f -})
  where
    f :: Word8 -> Double
    f byte = fromIntegral byte / 255

-------------------
-- Loading MNIST --
-------------------

loadMNIST :: IO RawMNIST
loadMNIST = do
  trainImg <- loadImg trainImgPath
  trainLabel <- loadLabel trainLabelPath
  testImg <- loadImg testImgPath
  testLabel <- loadLabel testLabelPath
  pure MNIST{..}

loadNormalizedMNIST :: IO NormalMNIST
loadNormalizedMNIST = normalize <$> loadMNIST

loadImg :: FilePath -> IO (Matrix Word8)
loadImg filePath =
  reshape imgSize . fromByteString . drop 16 <$> readFile filePath

loadLabel :: FilePath -> IO (Vector Word8)
loadLabel filePath =
  fromByteString . drop 8 <$> readFile filePath
