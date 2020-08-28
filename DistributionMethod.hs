module DistributionMethod where

import Distribution
import Predictor

type DistributionToMethod t y h d = DistributionEstimator t d -> Method t y h

binaryDistributionMethod :: (RealFloat y, Distribution d t) => DistributionToMethod t y () d
binaryDistributionMethod (name, estimator) (_, dat) = ("Bin:"++name, pred)
    where sel f = map fst $ filter (\(_, y) -> f y) dat
          len = fromIntegral . length
          pos = sel (>0.5)
          neg = sel (<=0.5)
          pred = predictByDist [0, 1] [(len neg, estimator neg), (len pos, estimator pos)]

