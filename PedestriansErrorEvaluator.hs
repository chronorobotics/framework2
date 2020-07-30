module PedestriansErrorEvaluator where

import Predictor
import Cell

import Data.Sequence
import Data.Foldable

pedestriansErrorEvaluator :: (RealFrac x, RealFrac y) => ErrorFunc y -> ErrorEvaluator (Cell x) y (CellGrid x)
pedestriansErrorEvaluator err (_, pred) (grid, test) = (sum $ fmap (\(c_, y_) -> err (pred $ getSubcellById grid c_) y_) (Data.Sequence.zip (fromList [0..(cc-1)]) hist)) / (fromIntegral $ cc)
    where cc = subcellCount grid
          hist0 = Data.Sequence.replicate cc 0
          add h i y = update i ((index h i) + y) h
          adder ((p, _), y) h = add h (getSubcellId grid p) y
          hist = foldr adder hist0 test

