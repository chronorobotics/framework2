{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module WrappedCauchy where

import Data.Complex
import System.Random

import Distribution
import EMAlgorithm

data WrappedCauchy a = WrappedCauchy (Complex a)

instance RealFloat a => Distribution (WrappedCauchy a) a where
    densityAt (WrappedCauchy mu) t = realToFrac $ (1 - (magnitude mu)^2) / (2*pi * (magnitude $ (exp (0:+t)) - mu)^2)
    distributionShortcut _ = "wC"

instance RealFloat a => EMDistribution (WrappedCauchy a) a where
    maximumLikelihoodEstimate dat = WrappedCauchy $ fmap realToFrac $ i' (0:+0)
        where u z phi = (z - phi) / (1 - (conjugate phi)*z)
              sum_w = (realToFrac $ sum $ map snd dat) :+ 0
              f mu_n = u ((sum $ map (\(t, w) -> (w:+0)*(u (exp (0:+(realToFrac t))) mu_n)) dat)/sum_w) (-mu_n)
              i a b | a == b = a
                    | otherwise = i b $ f b
              i' a = i a $ f a

initWrappedCauchy :: (RandomGen g, RealFloat a, Random a) => a -> InitDistribution g (WrappedCauchy a)
initWrappedCauchy max_mu g = (WrappedCauchy ((r*(cos phi)) :+ (r*(sin phi))), g'')
    where (r, g') = randomR (0, max_mu) g
          (phi, g'') = randomR (-pi, pi) g'

wrappedCauchyMaxMu :: (RealFloat a) => a -> WrappedCauchy a -> WrappedCauchy a
wrappedCauchyMaxMu max_mu (WrappedCauchy mu) | mag < max_mu = WrappedCauchy mu
                                             | otherwise = WrappedCauchy $ fmap (/(mag*max_mu)) mu
    where mag = magnitude mu

