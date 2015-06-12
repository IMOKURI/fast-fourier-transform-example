
module Main where

import Data.Array.CArray
import qualified Math.FFT as FFT
import System.Random
import Data.Complex
import Foreign.Storable.Complex()

main :: IO ()
main = do
  let x = take 1024 $ randomRs (-1, 1) $ mkStdGen 1 :: [Double]
      ax= array (1, 1024) $ zip [1..] [i:+0|i<-x] :: CArray Int (Complex Double)
  print $ assocs $ FFT.dft $ mapArray (hamming 1024) ax

mapArray :: (Int -> Complex Double -> Complex Double) -> CArray Int (Complex Double) -> CArray Int (Complex Double)
mapArray f a = array (bounds a) $ map (\(i, v) -> (i, f i v)) $ assocs a

hamming :: Int -> Int -> Complex Double -> Complex Double
hamming n i v = v * (0.53836 - 0.46164 * cos((2.0 * pi * fromIntegral i) / (fromIntegral n - 1)));

