module DataCollector where

import Predictor
import Cell

import Data.Sequence
import Data.Foldable

dataCollector :: RealFrac t => y -> (y -> y -> y) -> Dataset [t] y (CellGrid t, h) -> Dataset [t] y h
dataCollector zero sum_f ((grid, header), dat) = (header, dat')
    where cc = subcellCount grid
          hist0 = Data.Sequence.replicate cc zero
          add h i y = update i (sum_f (index h i) y) h
          hist = foldr (\(t, y) h -> add h (getSubcellId grid t) y) hist0 dat
          dat' = map (\(i, y) -> ((p1 $ getSubcellById grid i), y)) (Prelude.zip [0..(cc-1)] (toList hist))
          p1 (x, _) = x

