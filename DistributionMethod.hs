module DistributionMethod where

import Distribution
import Predictor

type DistributionToMethod t y h d = DistributionEstimator t d -> Method t y h

createDistributionMethod :: (p1 -> DistributionToMethod t y h d) -> (p2 -> DistributionEstimator t d) -> MethodWithParams (p1, p2) t y h
createDistributionMethod dtm de (p1, p2) = dtm p1 (de p2)

binaryDistributionMethod :: (RealFloat y, Distribution d t) => DistributionToMethod t y () d
binaryDistributionMethod (name, estimator) (_, dat) = ("Bin:"++name, pred)
    where sel f = map fst $ filter (\(_, y) -> f y) dat
          len = fromIntegral . length
          pos = sel (>0.5)
          neg = sel (<=0.5)
          pred = predictByDist [0, 1] [(len neg, estimator neg), (len pos, estimator pos)]

