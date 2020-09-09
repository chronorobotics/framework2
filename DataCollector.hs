module DataCollector where

import Predictor
import Cell

import Data.Sequence
import Data.Foldable

-- |This function filters the dataset in a way so it sums all data in each cell
-- and each of these sums is used as a single piece of data. The first argument
-- is the zero element and the second argument is the sum function. 0 and (+)
-- shall be used for classic summation of numbers.
dataCollector :: RealFrac t => y -> (y -> y -> y) -> Dataset [t] y (CellGrid t, h) -> Dataset [t] y h
dataCollector zero sum_f ((grid, header), dat) = (header, dat')
    where cc = subcellCount grid
          hist0 = Data.Sequence.replicate cc zero
          add h i y = update i (sum_f (index h i) y) h
          hist = foldl (\h (t, y) -> add h (getSubcellId grid t) y) hist0 dat
          dat' = map (\(i, y) -> ((p1 $ getSubcellById grid i), y)) (Prelude.zip [0..(cc-1)] (toList hist))
          p1 (x, _) = x

