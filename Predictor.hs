module Predictor where

type Dataset t y = [(t, y)]
type Predictor t y = (String, t -> y)
type ErrorFunc y = y -> y -> Float

type Method t y = Dataset t y -> Predictor t y
type MethodWithParams p t y = p -> Method t y

meanError :: Predictor t y -> Dataset t y -> ErrorFunc y -> Float
meanError (_, pred) test err = (sum $ map (\(t_, y_) -> err (pred t_) y_) test) / (fromIntegral $ length test)

bruteForceTrain :: MethodWithParams p t y -> [p] -> ErrorFunc y -> Method t y
bruteForceTrain mwp params err = bestm
    where bestm dat = best
              where preds = map (\p -> mwp p dat) params
                    errs = map (\pr -> meanError pr dat err) preds
                    s = zip errs preds
                    (_, best) = foldr better (head s) (tail s)
                    better (e1, p1) (e2, p2) | e2 > e1 = (e1, p1)
                                             | otherwise = (e2, p2)

