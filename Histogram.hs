module Histogram where

import Data.Fixed
import Data.Sequence
import Predictor
import Mean

getBinNo :: (RealFrac t, Integral n) => t -> t -> n -> n
getBinNo period time nbins = floor ((mod' time period) / bin_length)
    where bin_length = period / (fromIntegral nbins)

histogramPredictor :: (RealFrac t, Show y) => Seq y -> t -> String -> Predictor t y
histogramPredictor bins period str = (str, pred)
    where pred t = index bins (getBinNo period t (Data.Sequence.length bins))

histogramMethod :: (RealFrac t, Show y, Meanable y) => MethodWithParams (t, Int) t y h
histogramMethod (period, nbins) (_, dat) = histogramPredictor (fmap (\(s,n) -> meanDivision s n) bins) period ("Hist "++(show nbins))
    where s = Data.Sequence.replicate nbins (meanZero, 0)
          addToBin bs i y = update i (add (index bs i) y) bs
              where add (s, n) y = (meanSum s y, n + 1)
          bins = foldr (\(t, y) bs -> addToBin bs (getBinNo period t nbins) y) s dat

