module DistributionMethod where

import Distribution
import Predictor
import EMAlgorithm
import System.Random

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

convexDistributionMethod :: (RealFloat y, Random y, Show y, Distribution d t, EMDistribution d' y, MeanDistribution d' y) => EMParams d' y y -> DistributionToMethod t y () d
convexDistributionMethod emParams (name, estimator) (_, dat) = ("Conv "++(show cc)++":"++name, pred)
    where (_, em) = emAlgorithm emParams
          clusters = em $ map snd dat
          cc = length clusters
          means = map (distributionMean . snd) clusters
          ws_at p = map (\(w, d) -> w * (densityAt d p)) clusters
          max_w p = snd $ foldl mf (0, 0) $ zip (ws_at p) [0..(cc-1)]
          mf (max_val, max_id) (val, id) | val > max_val = (val, id)
                                         | otherwise = (max_val, max_id)
          d' = map (\(t, y) -> (t, max_w y)) dat
          tss = map (\i -> map fst $ filter (\(t, c) -> c == i) d') [0..(cc-1)]
          pred = predictByDist means $ map (\ts -> (fromIntegral $ length ts, estimator ts)) tss

