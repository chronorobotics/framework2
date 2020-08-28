{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module EMAlgorithm where

import Distribution
import System.Random

class (Distribution dist space) => EMDistribution dist space | dist -> space where
    maximumLikelihoodEstimate :: RealFloat f => [(space, f)] -> dist

-- cartesian product of distributions
instance (EMDistribution d1 s1, EMDistribution d2 s2) => EMDistribution (d1, d2) (s1, s2) where
    maximumLikelihoodEstimate dat = (maximumLikelihoodEstimate dat1, maximumLikelihoodEstimate dat2)
        where dat1 = map (\((p1, _), w) -> (p1, w)) dat
              dat2 = map (\((_, p2), w) -> (p2, w)) dat

expectation :: (RealFloat f, EMDistribution d s) => [s] -> [(f, d)] -> [[f]]
expectation points clusters = map e1 points
    where e1 p = normalise $ map (\(w, d) -> w * densityAt d p) clusters
          normalise xs = map (/(sum xs)) xs

maximisation :: (RealFloat f, EMDistribution d s) => [s] -> [[f]] -> [(f, d)]
maximisation points alphass = zip weights clusters
    where k = length $ alphass !! 0
          n = fromIntegral $ length alphass
          weights = map (\i -> (sum $ map (!!i) alphass) / n) [0..(k-1)]
          clusters = map (\i -> maximumLikelihoodEstimate $ zip points $ map (!!i) alphass) [0..(k-1)]

performEM :: (RealFloat f, EMDistribution d s) => [(f, d)] -> f -> (d -> d) -> [s] -> [(f, d)]
performEM start min_dl bounder points = em start (logLikelihood points start)
    where bounder' wcs = map (\(w, c) -> (w, bounder c)) wcs
          em' = bounder' . (maximisation points) . (expectation points)
          em wcs lh | lh' - lh < min_dl = wcs'
                    | otherwise = em wcs' lh'
              where wcs' = em' wcs
                    lh' = logLikelihood points wcs'

initEM :: (RandomGen g, Distribution d s, RealFloat f, Random f) => InitDistribution g d -> Int -> InitDistribution g [(f, d)]
initEM init n gen = (map (\(w, c) -> (w/sum_w, c)) wcs, gen')
    where add_wc (wcs, g) = ((w, c):wcs, g'')
              where (c, g') = init g
                    (w, g'') = randomR (0.0, 1.0) g'
          (wcs, gen') = iterate add_wc ([], gen) !! n
          sum_w = sum $ map (\(w, _) -> w) wcs

emAlgorithm :: (Random f, RealFloat f, EMDistribution d s) => Int -> Int -> InitDistribution StdGen d -> f -> (d -> d) -> DistributionEstimator s [(f, d)]
emAlgorithm clusters seed init min_dl bounder = ("EM-"++(distributionShortcut $ snd $ start !! 0)++" "++(show clusters), performEM start min_dl bounder)
    where (start, _) = initEM init clusters $ mkStdGen seed

