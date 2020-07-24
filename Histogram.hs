module Histogram where

import Data.Fixed
import Predictor
import Mean

getBinNo :: (RealFrac t, Integral n) => t -> t -> n -> n
getBinNo period time nbins = floor ((mod' time period) / bin_length)
    where bin_length = period / (fromIntegral nbins)

histogramPredictor :: RealFrac t => [y] -> t -> String -> Predictor t y
histogramPredictor bins period str = (str, pred)
    where pred t = bins !! (getBinNo period t (length bins))

histogramMethod :: (RealFrac t, Meanable y) => MethodWithParams (t, Int) t y
histogramMethod (period, nbins) dat = histogramPredictor (map mean bins) period ("Hist "++(show nbins))
    where s = replicate nbins []
          addToBin bs n y = b1 ++ [b ++ [y]] ++ b2
              where (b1, (b:b2)) = splitAt n bs
          bins = foldr (\(t, y) bs -> addToBin bs (getBinNo period t nbins) y) s dat

