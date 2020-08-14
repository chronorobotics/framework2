module Trimmer where

import Predictor

trimmer :: Ord y => y -> y -> Method t y h -> Method t y h
trimmer lower upper method dat = (name, pred')
    where (name, pred) = method dat
          pred' t | pred t < lower = lower
                  | pred t > upper = upper
                  | otherwise = pred t

