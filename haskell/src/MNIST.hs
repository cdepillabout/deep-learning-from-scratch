
module MNIST where

import Prelude hiding (dropWhile)

import Conduit
       (Consumer, Producer, (=$=), ($$), await, concatC, dropC, printC, mapC,
        sinkVector, sourceFile)
import Control.Monad (void)
import Control.Monad.Trans.Resource (MonadResource, runResourceT)
import Data.ByteString (ByteString)
import Data.Word (Word8)
import Numeric.LinearAlgebra (Vector)
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
  testImg <- loadImg testImgPath
  pure (trainImg, [], testImg, [])

-- TODO: Maybe this would be safer if I used Numeric.LinearAlgebra.fromByteString??

loadImg :: FilePath -> IO (Vector Word8)
loadImg filePath =
  runResourceT $
     source =$= concatC $$ dropC 16 >> sinkVector
  where
    source :: MonadResource m => Producer m ByteString
    source = sourceFile filePath

loadImg' :: FilePath -> IO ()
loadImg' filePath =
  void $ runResourceT $
     -- source =$= concatC $$ dropC 16 >> sinkVector
     source =$= concatC $$ loop
  where
    source :: MonadResource m => Producer m ByteString
    source = sourceFile filePath

    loop :: Monad m => Consumer a m ()
    loop = await >>= \case
      Nothing -> pure ()
      Just _ -> loop
