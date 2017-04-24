
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

data MNISTContainer a = MNIST
  { trainImg :: !a
  , trainLabel :: !(Vector Word8)
  , testImg :: !a
  , testLabel :: !(Vector Word8)
  } deriving (Eq, Read, Show)

instance Functor MNISTContainer where
  fmap :: (a -> b) -> MNISTContainer a -> MNISTContainer b
  fmap f mnist =
    MNIST
    { trainImg = f $ trainImg mnist
    , trainLabel = trainLabel mnist
    , testImg = f $ testImg mnist
    , testLabel = testLabel mnist
    }

type RawMNIST = MNISTContainer (Vector Word8)

type DoubleMNIST = MNISTContainer (Vector Double)

type MNIST = MNISTContainer (Matrix Double)

normalize :: MNIST -> MNIST
normalize = fmap (cmap f)
  where
    f :: Double -> Double
    f byte = byte / 255

doubleize :: RawMNIST -> DoubleMNIST
doubleize = fmap (map fromIntegral)

-------------------
-- Loading MNIST --
-------------------

loadMNISTRaw :: IO RawMNIST
loadMNISTRaw = do
  trainImg <- loadImg trainImgPath
  trainLabel <- loadLabel trainLabelPath
  testImg <- loadImg testImgPath
  testLabel <- loadLabel testLabelPath
  pure MNIST{..}

-- | This is a non-normalized, flattened MNIST, with one_hot_label = false.
loadMNIST :: IO MNIST
loadMNIST = fmap (reshape imgSize) . doubleize <$> loadMNISTRaw

-- | This is a Normalized, flattened MNIST, with one_hot_label = false.
loadNormalizedMNIST :: IO MNIST
loadNormalizedMNIST = normalize <$> loadMNIST

loadImg :: FilePath -> IO (Vector Word8)
loadImg filePath =
  fromByteString . drop 16 <$> readFile filePath

loadLabel :: FilePath -> IO (Vector Word8)
loadLabel filePath =
  fromByteString . drop 8 <$> readFile filePath
