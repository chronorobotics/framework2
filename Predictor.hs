module Predictor where

type Dataset t y = [(t, y)]
type Predictor t y = (String, t -> y)
type ErrorFunc y = y -> y -> Float
type ErrorEvaluator t y = Predictor t y -> Dataset t y -> Float

type Method t y = Dataset t y -> Predictor t y
type MethodWithParams p t y = p -> Method t y

meanError :: ErrorFunc y -> ErrorEvaluator t y
meanError err (_, pred) test = (sum $ map (\(t_, y_) -> err (pred t_) y_) test) / (fromIntegral $ length test)

bruteForceTrain :: MethodWithParams p t y -> [p] -> ErrorEvaluator t y -> Method t y
bruteForceTrain mwp params err = bestm
    where bestm dat = best
              where preds = map (\p -> mwp p dat) params
                    errs = map (\pr -> err pr dat) preds
                    s = zip errs preds
                    (_, best) = foldr better (head s) (tail s)
                    better (e1, p1) (e2, p2) | e2 > e1 = (e1, p1)
                                             | otherwise = (e2, p2)

methodChangeType :: (t2 -> t1) -> (y2 -> y1) -> (y1 -> y2) -> Method t1 y1 -> Method t2 y2
methodChangeType ft21 fy21 f12 m1 = \dat -> tt $ m1 $ map f21 dat
    where tt (s, f) = (s, f12 . f . ft21)
          f21 (t, y) = (ft21 t, fy21 y)

