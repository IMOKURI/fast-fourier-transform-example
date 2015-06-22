module Main where

import Functions

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

numOfData :: Int -- Frequency
numOfData = 256

rawData :: [Double]
rawData = take numOfData $ randomRs (-1, 1) $ mkStdGen 1

inputData :: [(Int, Double)]
inputData = zip [1..] rawData

----------

cutoffFrequency :: Double
cutoffFrequency = 40.0

transBand :: Double
transBand = 10.0

filterDomain :: [Double]
filterDomain = [-(fromIntegral nof-1)/2 .. (fromIntegral nof-1)/2]
  where nof' = round $ 3.3 * (fromIntegral numOfData) / transBand
        nof = if odd nof' then nof' else nof'+1 :: Int

hamming :: Int -> Int -> Double -> Double
hamming n i v = v * (0.53836 - 0.46164 * cos((2.0 * pi * fromIntegral i) / (fromIntegral n - 1)));

lowPass :: [Double]
lowPass = zipWith (hamming (length filterDomain)) [1..] [ k * sinc (pi*k*n) | n <- filterDomain ]
  where k = 2 * cutoffFrequency / (fromIntegral numOfData)

lowPassedData :: [(Int, Double)]
lowPassedData = zip [1..] lpd
  where lpd = take numOfData $ drop (div (length filterDomain) 2) $ convolve lowPass rawData

----------

toDFT :: [(Int, Double)] -> [(Int, Complex Double)]
toDFT ds = take (div numOfData 2) $ assocs $ dft $ array (1, length ds) $ map (\(i, v) -> (i, v:+0)) ds

toAmplitude :: [(Int, Complex Double)] -> [(Int, Double)]
toAmplitude = map (\(i,z) -> (i, magnitude z))

----------

timeChart :: Renderable ()
timeChart = toRenderable timeLayout
  where timeLayout = layout_title .~ "Time Scale Data"
                   $ layout_plots .~ [ toPlot inputLine
                                     , toPlot lowPassLine ]
                   $ def

        inputLine = plot_lines_title .~ "Input Data"
                  $ plot_lines_style . line_color .~ opaque skyblue
                  $ plot_lines_values .~ [inputData]
                  $ def

        lowPassLine = plot_lines_title .~ "Low Passed Data"
                    $ plot_lines_style . line_color .~ opaque green
                    $ plot_lines_values .~ [lowPassedData]
                    $ def

frequencyChart :: Renderable ()
frequencyChart = toRenderable frequencyLayout
  where frequencyLayout = layout_title .~ "Frequency Scale Data"
                        $ layout_plots .~ [ toPlot inputLine'
                                          , toPlot lowPassLine' ]
                        $ def

        inputLine' = plot_lines_title .~ "Input Data"
                   $ plot_lines_style . line_color .~ opaque skyblue
                   $ plot_lines_values .~ [toAmplitude $ toDFT inputData]
                   $ def

        lowPassLine' = plot_lines_title .~ "Low Passed Data"
                     $ plot_lines_style . line_color .~ opaque green
                     $ plot_lines_values .~ [toAmplitude $ toDFT lowPassedData]
                     $ def

----------

main :: IO ()
main = do
  _ <- renderableToFile def "01_time_scale.png" timeChart
  _ <- renderableToFile def "02_frequency_scale.png" frequencyChart
  return ()

