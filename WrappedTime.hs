module WrappedTime where

import Data.Fixed

import Predictor

wrapTime' :: (RealFloat a) => a -> a -> a
wrapTime' period time = 2*pi * (mod' time period) / period

wrapTime :: (RealFloat a) => a -> a -> [a]
wrapTime period time = let phi = wrapTime' period time in [cos phi, sin phi]

wrapHyperTime' :: (RealFloat a) => [a] -> a -> [a]
wrapHyperTime' periods time = map (flip wrapTime' time) periods

wrapHyperTime :: (RealFloat a) => [a] -> a -> [a]
wrapHyperTime periods time = foldr (++) [] $ map (flip wrapTime time) periods

wrapTimeSpace :: (RealFloat a) => (b -> a -> [a]) -> b -> [a] -> [a]
wrapTimeSpace wrapper period (time:space) = (wrapper period time) ++ space

wrappedMethod :: (p -> t -> wt) -> p -> Method wt y h -> Method t y h
wrappedMethod wrap period m (header, dat) = wp $ m (header, dat')
    where dat' = map (\(t, y) -> (wrap period t, y)) dat
          wp (s, f) = (s, f . (wrap period))

