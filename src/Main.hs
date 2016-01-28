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

lowCutoffFrequency :: Double
lowCutoffFrequency = 40.0

highCutoffFrequency :: Double
highCutoffFrequency = 80.0

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
  where k = 2 * lowCutoffFrequency / (fromIntegral numOfData)

lowPassedData :: [(Int, Double)]
lowPassedData = zip [1..] lpd
  where lpd = take numOfData $ drop (div (length filterDomain) 2) $ convolve lowPass rawData

highPass :: [Double]
highPass = zipWith (hamming (length filterDomain)) [1..] [ sinc (pi*n) - k * sinc (pi*k*n) | n <- filterDomain ]
  where k = 2 * highCutoffFrequency / (fromIntegral numOfData)

highPassedData :: [(Int, Double)]
highPassedData = zip [1..] hpd
  where hpd = take numOfData $ drop (div (length filterDomain) 2) $ convolve highPass rawData

bandPass :: [Double]
bandPass = zipWith (hamming (length filterDomain)) [1..] [ kh * sinc (pi*kh*n) - kl * sinc (pi*kl*n) | n <- filterDomain ]
  where kl = 2 * lowCutoffFrequency / (fromIntegral numOfData)
        kh = 2 * highCutoffFrequency / (fromIntegral numOfData)

bandPassedData :: [(Int, Double)]
bandPassedData = zip [1..] bpd
  where bpd = take numOfData $ drop (div (length filterDomain) 2) $ convolve bandPass rawData

----------

toDFT :: [(Int, Double)] -> [(Int, Complex Double)]
toDFT ds = take (div numOfData 2) $ assocs $ dft $ array (1, length ds) $ map (\(i, v) -> (i, v:+0)) ds

toAmplitude :: [(Int, Complex Double)] -> [(Int, Double)]
toAmplitude = map (\(i,z) -> (i, magnitude z))

----------

timeChartLowPass :: Renderable ()
timeChartLowPass = toRenderable timeLayout
  where timeLayout = layout_title .~ "Time Scale Data(Low Pass)"
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

timeChartHighPass :: Renderable ()
timeChartHighPass = toRenderable timeLayout
  where timeLayout = layout_title .~ "Time Scale Data(High Pass)"
                   $ layout_plots .~ [ toPlot inputLine
                                     , toPlot highPassLine ]
                   $ def

        inputLine = plot_lines_title .~ "Input Data"
                  $ plot_lines_style . line_color .~ opaque skyblue
                  $ plot_lines_values .~ [inputData]
                  $ def

        highPassLine = plot_lines_title .~ "High Passed Data"
                     $ plot_lines_style . line_color .~ opaque orange
                     $ plot_lines_values .~ [highPassedData]
                     $ def

timeChartBandPass :: Renderable ()
timeChartBandPass = toRenderable timeLayout
  where timeLayout = layout_title .~ "Time Scale Data(Band Pass)"
                   $ layout_plots .~ [ toPlot inputLine
                                     , toPlot bandPassLine ]
                   $ def

        inputLine = plot_lines_title .~ "Input Data"
                  $ plot_lines_style . line_color .~ opaque skyblue
                  $ plot_lines_values .~ [inputData]
                  $ def

        bandPassLine = plot_lines_title .~ "Band Passed Data"
                     $ plot_lines_style . line_color .~ opaque purple
                     $ plot_lines_values .~ [bandPassedData]
                     $ def

frequencyChart :: Renderable ()
frequencyChart = toRenderable frequencyLayout
  where frequencyLayout = layout_title .~ "Frequency Scale Data"
                        $ layout_y_axis .~ yAxis'
                        $ layout_plots .~ [ toPlot inputLine'
                                          , toPlot lowPassLine'
                                          , toPlot highPassLine'
                                          , toPlot bandPassLine' ]
                        $ def

        yAxis' = laxis_title .~ "Amplitude"
               $ def

        inputLine' = plot_lines_title .~ "Input Data"
                   $ plot_lines_style . line_color .~ opaque skyblue
                   $ plot_lines_values .~ [toAmplitude $ toDFT inputData]
                   $ def

        lowPassLine' = plot_lines_title .~ "Low Passed Data"
                     $ plot_lines_style . line_color .~ opaque green
                     $ plot_lines_values .~ [toAmplitude $ toDFT lowPassedData]
                     $ def

        highPassLine' = plot_lines_title .~ "High Passed Data"
                      $ plot_lines_style . line_color .~ opaque orange
                      $ plot_lines_values .~ [toAmplitude $ toDFT highPassedData]
                      $ def

        bandPassLine' = plot_lines_title .~ "Band Passed Data"
                      $ plot_lines_style . line_color .~ opaque purple
                      $ plot_lines_values .~ [toAmplitude $ toDFT bandPassedData]
                      $ def

----------

main :: IO ()
main = do
  _ <- renderableToFile def "time_scale_lowpass.png" timeChartLowPass
  _ <- renderableToFile def "time_scale_highpass.png" timeChartHighPass
  _ <- renderableToFile def "time_scale_bandpass.png" timeChartBandPass
  _ <- renderableToFile def "frequency_scale.png" frequencyChart
  return ()

