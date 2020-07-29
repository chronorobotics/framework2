{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Mean where

import Predictor

class Meanable a where
    meanZero :: a
    meanSum :: a -> a -> a
    meanDivision :: a -> Int -> a
    mean :: [a] -> a
    mean as = meanDivision (foldr meanSum meanZero as) (length as)

instance Fractional a => Meanable a where
    meanZero = 0
    meanSum a b = a + b
    meanDivision a 0 = 0
    meanDivision a i = a / (fromIntegral i)

instance (Meanable a) => Meanable [a] where
    meanZero = []
    meanSum [] as = as
    meanSum as [] = as
    meanSum as bs = map (\(a,b) -> meanSum a b) (zip as bs)
    meanDivision a i = map (\a' -> meanDivision a' i) a
    mean as = map mean $ foldr (\a b -> map (\(a,b)->a:b) (zip a b)) (map (\a -> [a]) (head as)) (tail as)

constPredictor :: y -> String -> Predictor t y
constPredictor pred str = (str, \_ -> pred)

meanMethod :: Meanable y => Method t y h
meanMethod (_, dat) = constPredictor (mean $ map (\(_, a) -> a) dat) "Mean"

