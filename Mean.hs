{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Mean where

import Predictor

class Meanable a where
    mean :: [a] -> a

instance Fractional a => Meanable a where
    mean as = (sum as) / (fromIntegral $ length as)

instance (Meanable a) => Meanable [a] where
    mean as = map mean $ foldr (\a b -> map (\(a,b)->a:b) (zip a b)) (map (\a -> [a]) (head as)) (tail as)

constPredictor :: y -> String -> Predictor t y
constPredictor pred str = (str, \_ -> pred)

meanMethod :: Meanable y => Method t y
meanMethod dat = constPredictor (mean $ map (\(_, a) -> a) dat) "Mean"

