
module Main where

import Data.Array.CArray
import Math.FFT
import System.Random
import Data.Complex
import Foreign.Storable.Complex()

import Control.Lens
import Data.Colour
import Data.Colour.Names
import Data.Default.Class
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Cairo


main :: IO ()
main = do
  _ <- renderableToFile def "01_input_data.png" inputChart
  _ <- renderableToFile def "02_dft_of_input_data.png" dftChart
  return ()

sampleN :: Int
sampleN = 32

randomData :: [Double]
randomData = take sampleN $ randomRs (-1, 1) $ mkStdGen 1

mapArray :: (Int -> Complex Double -> Complex Double) -> CArray Int (Complex Double) -> CArray Int (Complex Double)
mapArray f a = array (bounds a) $ map (\(i, v) -> (i, f i v)) $ assocs a

hamming :: Int -> Int -> Complex Double -> Complex Double
hamming n i v = v * (0.53836 - 0.46164 * cos((2.0 * pi * fromIntegral i) / (fromIntegral n - 1)));

inputChart :: Renderable ()
inputChart = toRenderable inputLayout
  where inputLayout = layout_title .~ "Input Data"
                    $ layout_plots .~ [toPlot inputLine]
                    $ def

        inputLine = plot_lines_title .~ "Input Data"
                  $ plot_lines_style . line_color .~ opaque blue
                  $ plot_lines_values .~ ([zip [1..] randomData] :: [[(Int, Double)]])
                  $ def

dftChart :: Renderable ()
dftChart = toRenderable dftLayout
  where dftLayout = layoutlr_title .~ "DFT of Input Data"
                  $ layoutlr_left_axis . laxis_override .~ axisGridHide
                  $ layoutlr_right_axis . laxis_override .~ axisGridHide
                  $ layoutlr_x_axis . laxis_override .~ axisGridHide
                  $ layoutlr_plots .~ [ Left (toPlot dftMagnitudeLine)
                                      , Right (toPlot dftPhaseLine) ]
                  $ def

        dftMagnitudeLine = plot_lines_title .~ "Magnitude"
                         $ plot_lines_style . line_color .~ opaque blue
                         $ plot_lines_values .~ [magnitudeData]
                         $ def

        dftPhaseLine = plot_lines_title .~ "Phase"
                     $ plot_lines_style . line_color .~ opaque green
                     $ plot_lines_values .~ [phaseData]
                     $ def

        ax = array (1, sampleN) $ zip [1..] [i:+0|i<-randomData] :: CArray Int (Complex Double)
        dftData = assocs $ dft $ mapArray (hamming sampleN) ax
        magnitudeData = map (\(i,z) -> (i, magnitude z)) dftData
        phaseData = map (\(i,z) -> (i, phase z)) dftData

