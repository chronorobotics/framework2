{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module VonMises where

import System.Random
import Numeric.GSL.Special.Bessel
import Numeric.GSL.Root

import Distribution
import EMAlgorithm

import Debug.Trace

tr x = trace (show x) x

data VonMises = VonMises Double Double deriving (Show, Eq)

instance Distribution VonMises Double where
    densityAt (VonMises mu kappa) t = realToFrac $ (exp $ kappa * (cos $ t - mu)) / (2*pi * bessel_I0 kappa)
    distributionShortcut _ = "vM"

instance EMDistribution VonMises Double where
    maximumLikelihoodEstimate ps ws = VonMises mu kappa
        where ws' = map realToFrac ws
              sum_w = sum ws'
              x = (sum $ zipWith (*) ws' $ map cos ps) / sum_w
              y = (sum $ zipWith (*) ws' $ map sin ps) / sum_w
              mu = atan2 y x
              r = sqrt $ x^2 + y^2
              f x = (bessel_I1 x) / (bessel_I0 x) - r
              kappa | f 708 < 0 = 708
                    | otherwise = let (k, _) = uniRoot Bisection 1E-7 30 f 0 708 in k

initVonMises :: (RandomGen g) => Double -> InitDistribution g VonMises
initVonMises max_kappa g = (VonMises mu kappa, g'')
    where (kappa, g') = randomR (0, max_kappa) g
          (mu, g'') = randomR (-pi, pi) g'

vonMisesMaxKappa :: Double -> VonMises -> VonMises
vonMisesMaxKappa max_kappa (VonMises mu kappa) = VonMises mu (min kappa max_kappa)

