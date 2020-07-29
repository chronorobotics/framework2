module PedestriansErrorEvaluator where

import Predictor
import Cell

pedestriansErrorEvaluator :: (RealFrac x, RealFrac y) => ErrorFunc y -> ErrorEvaluator (Cell x) y (CellGrid x)
pedestriansErrorEvaluator err (_, pred) (grid, test) = (sum $ map (\(c_, y_) -> err (pred $ getSubcellById grid c_) y_) (zip [0..(cc-1)] hist)) / (fromIntegral $ cc)
    where cc = subcellCount grid
          hist0 = replicate cc 0
          add h i y = h1 ++ [(head h2) + y] ++ (tail h2)
              where (h1, h2) = splitAt i h
          adder ((p, _), y) h = add h (getSubcellId grid p) y
          hist = foldr adder hist0 test

