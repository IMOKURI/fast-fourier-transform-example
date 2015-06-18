module Main where

import Math.FFT
import Data.Array.CArray
import Data.Complex
import Foreign.Storable.Complex()

import Control.Lens
import Data.Colour
import Data.Colour.Names
import Data.Default.Class
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Cairo

import System.Random

----------

sampleN :: Int
sampleN = 256

randomData :: [Double]
randomData = take sampleN $ randomRs (-1, 1) $ mkStdGen 1

----------

inputData :: [(Int, Double)]
inputData = zip [1..] randomData

windowingData :: [(Int, Double)]
windowingData = map (\(i, v) -> (i, hamming sampleN i v)) inputData

hamming :: Int -> Int -> Double -> Double
hamming n i v = v * (0.53836 - 0.46164 * cos((2.0 * pi * fromIntegral i) / (fromIntegral n - 1)));

----------

toDFT :: [(Int, Double)] -> [(Int, Complex Double)]
toDFT = assocs . dft . array (1, sampleN) . map (\(i, v) -> (i, v:+0))

toAmplitude :: [(Int, Complex Double)] -> [(Int, Double)]
toAmplitude = map (\(i,z) -> (i, magnitude z))

-- toPhase :: [(Int, Complex Double)] -> [(Int, Double)]
-- toPhase = map (\(i,z) -> (i, phase z))

----------

timeChart :: Renderable ()
timeChart = toRenderable timeLayout
  where timeLayout = layout_title .~ "Time Scale Data"
                   $ layout_plots .~ [ toPlot inputLine
                                     , toPlot windowingLine ]
                   $ def

        inputLine = plot_lines_title .~ "Input Data"
                  $ plot_lines_style . line_color .~ opaque blue
                  $ plot_lines_values .~ [inputData]
                  $ def

        windowingLine = plot_lines_title .~ "Windowing Data"
                      $ plot_lines_style . line_color .~ opaque green
                      $ plot_lines_values .~ [windowingData]
                      $ def

frequencyChart :: Renderable ()
frequencyChart = toRenderable frequencyLayout
  where frequencyLayout = layout_title .~ "Frequency Scale Data"
                        $ layout_plots .~ [ toPlot inputLine'
                                          , toPlot windowingLine' ]
                        $ def

        inputLine' = plot_lines_title .~ "Input Data"
                   $ plot_lines_style . line_color .~ opaque blue
                   $ plot_lines_values .~ [toAmplitude $ toDFT inputData]
                   $ def

        windowingLine' = plot_lines_title .~ "Windowing Data"
                       $ plot_lines_style . line_color .~ opaque green
                       $ plot_lines_values .~ [toAmplitude $ toDFT windowingData]
                       $ def

----------

main :: IO ()
main = do
  _ <- renderableToFile def "01_time_scale.png" timeChart
  _ <- renderableToFile def "02_frequency_scale.png" frequencyChart
  return ()

