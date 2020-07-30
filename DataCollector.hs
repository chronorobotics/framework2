module DataCollector where

import Predictor
import Cell

dataCollector :: RealFrac t => y -> (y -> y -> y) -> Dataset [t] y (CellGrid t, h) -> Dataset [t] y h
dataCollector zero sum_f ((grid, header), dat) = (header, dat')
    where cc = subcellCount grid
          hist0 = replicate cc zero
          add h i y = h1 ++ [sum_f (head h2) y] ++ (tail h2)
              where (h1, h2) = splitAt i h
          hist = foldr (\(t, y) h -> add h (getSubcellId grid t) y) hist0 dat
          dat' = map (\(i, y) -> ((p1 $ getSubcellById grid i), y)) (zip [0..(cc-1)] hist)
          p1 (x, _) = x

