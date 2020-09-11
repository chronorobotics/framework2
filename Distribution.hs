{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Distribution where

import Predictor
import System.Random

class Distribution dist space | dist -> space where
    densityAt :: RealFloat f => dist -> space -> f
    distributionShortcut :: dist -> String

class (Distribution dist space) => MeanDistribution dist space | dist -> space where
    distributionMean :: dist -> space

type InitDistribution g d = g -> (d, g)
type DistributionEstimator s d = (String, [s] -> d)

-- mixture of distributions
instance (Distribution d s, RealFloat f) => Distribution [(f, d)] s where
    densityAt wcs p = realToFrac $ sum $ map (\(w, c) -> w * densityAt c p) wcs
    distributionShortcut wcs = "[" ++ (distributionShortcut $ snd $ wcs !! 0) ++ "]"

-- cartesian product of distributions
instance (Distribution d1 s1, Distribution d2 s2) => Distribution (d1, d2) (s1, s2) where
    densityAt (d1, d2) (p1, p2) = (densityAt d1 p1) * (densityAt d2 p2)
    distributionShortcut (d1, d2) = (distributionShortcut d1) ++ "×" ++ (distributionShortcut d2)

logLikelihood :: (RealFloat f, Distribution d s) => [s] -> d -> f
logLikelihood points dist = sum $ map (\p -> log $ densityAt dist p) points

mergeInits :: RandomGen g => InitDistribution g d1 -> InitDistribution g d2 -> InitDistribution g (d1, d2)
mergeInits init1 init2 gen = ((d1, d2), gen'')
    where (d1, gen') = init1 gen
          (d2, gen'') = init2 gen'

predictByDist :: (Distribution d s, RealFloat f) => [f] -> [(f, d)] -> s -> f
predictByDist centres dists at = (sum $ zipWith (*) centres wds) / (sum wds)
    where ws = map fst dists
          ds = map (\(_, d) -> densityAt d at) dists
          wds = zipWith (*) ws ds

