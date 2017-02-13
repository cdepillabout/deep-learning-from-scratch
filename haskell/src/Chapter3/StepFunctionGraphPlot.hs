
module Chapter3.StepFunctionGraphPlot where

import ClassyPrelude hiding (Vector, (<.>), point)

import Data.Colour (Colour)
import Graphics.Rendering.Plot
       (Glyph(Cross), Figure, FormattedSeries, Scale(Linear), Series,
        blue, line, point, red, subtitle, title, xlabel, ylabel, yrange)
import Graphics.Rendering.Plot.Figure.Simple (plot)
import Graphics.Rendering.Plot.Gtk (display)
import Numeric.GSL.Histogram ({- stddev -})
import Numeric.LinearAlgebra
       (RandDist(Gaussian), Vector, linspace, randomVector)

stepFunction :: Double -> Double
stepFunction x = if x > 0 then 1 else 0

----------
-- plot --
----------

plotStepFunctionPlot :: IO ()
plotStepFunctionPlot = void $ display test_graph2

ln :: Int
ln = 25

ts :: Vector Double
ts = linspace ln (0,1)

rs :: Vector Double
rs = randomVector 0 Gaussian ln

ss :: Vector Double
ss = sin (15*2*pi*ts)

ds :: Vector Double
ds = 0.25*rs + ss

es :: Vector Double
es = error "this doesn't work" -- konst (0.25*(stddev rs)) ln

fs :: Double -> Double
fs = sin . (15*2*pi*)

test_graph2 :: Figure ()
test_graph2 = do
  plot
    ( ts :: Series
    , [ point (ds, es) (Cross, red :: Colour Double)
      , line fs (blue :: Colour Double)
      ] :: [FormattedSeries])
  title "Testing plot package:"
  subtitle "with 1 second of a 15Hz sine wave"
  xlabel "time (s)"
  ylabel "amplitude"
  yrange Linear (-1.25) 1.25
