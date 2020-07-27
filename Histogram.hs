module Histogram where

import Data.Fixed
import Predictor
import Mean

import Debug.Trace

getBinNo :: (RealFrac t, Integral n) => t -> t -> n -> n
getBinNo period time nbins = floor ((mod' time period) / bin_length)
    where bin_length = period / (fromIntegral nbins)

histogramPredictor :: (RealFrac t, Show y) => [y] -> t -> String -> Predictor t y
histogramPredictor bins period str = (str, pred)
    where pred t = bins !! (getBinNo period t (length bins))

histogramMethod :: (RealFrac t, Show y, Meanable y) => MethodWithParams (t, Int) t y
histogramMethod (period, nbins) dat = histogramPredictor (map (\(s,n) -> meanDivision s n) bins) period ("Hist "++(show nbins))
    where s = replicate nbins (meanZero, 0)
          addToBin bs n y = b1 ++ [add b y] ++ b2
              where (b1, (b:b2)) = splitAt n bs
                    add (s, n) y = (meanSum s y, n + 1)
          bins = foldr (\(t, y) bs -> addToBin bs (getBinNo period t nbins) y) s dat

