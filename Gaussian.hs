{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Gaussian where

import Numeric.LinearAlgebra hiding (Gaussian)
import Numeric.LinearAlgebra.Data
import Data.Complex
import System.Random
import EMAlgorithm
import Distribution

import Debug.Trace

mtr x = trace (show x) x

data Gaussian = Gaussian (Vector Double) (Vector Double, Matrix Double) deriving (Show, Eq)

instance Distribution Gaussian [Double] where
    densityAt (Gaussian mu (lambda, v)) at = realToFrac $ (exp x) / (sqrt $ (2*pi)^n * (product ls))
        where t = fromList $ zipWith (-) at $ toList mu
              t' = toList $ t <# v
              ls = toList lambda
              x = (-0.5) * (sum $ zipWith (/) (map (^2) t') ls)
              n = fromIntegral $ length at
    distributionShortcut _ = "Gauss"

instance EMDistribution Gaussian [Double] where
    maximumLikelihoodEstimate ps ws | n == 0 = Gaussian (fromList []) (fromList [], fromLists [])
                                    | otherwise = Gaussian (fromList means) ((\(u, s, v) -> (s, v)) $ svd $ fromLists cov)
        where n = length $ head ps
              dat = zip ps ws
              fromComplex' m = cmap (\(x :+ _) -> x) m
              fromComplex (a, b) = (fromComplex' a, fromComplex' b)
              sum_w = realToFrac $ sum ws
              rng = [0..(n-1)]
              means = map (\i -> (sum $ map (\(x, w) -> (realToFrac w) * (x!!i)) dat) / sum_w) rng
              cov = map (\i -> map (\j -> (sum $ map (\(x, w) -> (realToFrac w) * ((x!!i) - (means!!i)) * ((x!!j) - (means!!j))) dat) / sum_w) rng) rng

instance MeanDistribution Gaussian [Double] where
    distributionMean (Gaussian mu _) = toList mu

gaussianMinVariance :: Double -> Gaussian -> Gaussian
gaussianMinVariance limit (Gaussian mu (lambda, v)) = Gaussian mu (cmap clip lambda, v)
    where clip l | l < limit = limit
                 | otherwise = l

initGaussian :: (RandomGen g) => [Double] -> [Double] -> [Double] -> [Double] -> InitDistribution g Gaussian
initGaussian mean_min mean_max var_min var_max gen = (Gaussian (fromList means) (fromList lambda, ident $ length mean_min), gen'')
    where rv [] _ r g = (r, g)
          rv (mi:mis) (ma:mas) r g = let (r', g') = randomR (mi, ma) g in rv mis mas (r':r) g'
          (means, gen') = rv mean_min mean_max [] gen
          (lambda, gen'') = rv var_min var_max [] gen'

initGaussian' :: (RandomGen g) => Int -> Double -> Double -> Double -> Double -> InitDistribution g Gaussian
initGaussian' n mean_min mean_max var_min var_max = initGaussian (r mean_min) (r mean_max) (r var_min) (r var_max)
    where r = replicate n

gaussianMinVar :: Double -> Gaussian -> Gaussian
gaussianMinVar mv (Gaussian mu (lambda, v)) = Gaussian mu (cmap (max mv) lambda, v)

