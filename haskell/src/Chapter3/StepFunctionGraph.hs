
module Chapter3.StepFunctionGraph where

import ClassyPrelude hiding (Vector, (<.>))

import Control.Lens ((.=))

import Graphics.Gnuplot.Simple (Attribute(..), plotFunc)

import Graphics.Rendering.Chart.Easy
       (layout_margin, line, plot)
import Graphics.Rendering.Chart.Gtk (toWindow)

stepFunction :: Double -> Double
stepFunction x = if x > 0 then 1 else 0

plotStepFunctionGnu :: IO ()
plotStepFunctionGnu = do
  let x = [-5.0, -4.9 .. 5.0]
  plotFunc [YRange (-0.2, 1.2)] x stepFunction

plotStepFunctionChart :: IO ()
plotStepFunctionChart = do
  let x = [-5.0, -4.9 .. 5.0]
  toWindow 100 200 $ do
    -- TODO: Figure out how to get a bigger space around axis??
    layout_margin .= 200
    plot $ line "hello" [fmapZip stepFunction x]

fmapZip :: Functor f => (a -> b) -> f a -> f (a, b)
fmapZip f xs = fmap (id &&& f) xs

