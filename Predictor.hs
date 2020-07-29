module Predictor where

type Dataset t y h = (h, [(t, y)])
type Predictor t y = (String, t -> y)
type ErrorFunc y = y -> y -> Float
type ErrorEvaluator t y h = Predictor t y -> Dataset t y h -> Float

type Method t y h = Dataset t y h -> Predictor t y
type MethodWithParams p t y h = p -> Method t y h

meanError :: ErrorFunc y -> ErrorEvaluator t y h
meanError err (_, pred) (_, test) = (sum $ map (\(t_, y_) -> err (pred t_) y_) test) / (fromIntegral $ length test)

bruteForceTrain :: MethodWithParams p t y h -> [p] -> ErrorEvaluator t y h -> Method t y h
bruteForceTrain mwp params err = bestm
    where bestm dat = best
              where preds = map (\p -> mwp p dat) params
                    errs = map (\pr -> err pr dat) preds
                    s = zip errs preds
                    (_, best) = foldr better (head s) (tail s)
                    better (e1, p1) (e2, p2) | e2 > e1 = (e1, p1)
                                             | otherwise = (e2, p2)

methodChangeType :: (t2 -> t1) -> (y2 -> y1) -> (y1 -> y2) -> (h2 -> h1) -> Method t1 y1 h1 -> Method t2 y2 h2
methodChangeType ft21 fy21 f12 fh m1 = \(header, dat) -> tt $ m1 $ (fh header, map f21 dat)
    where tt (s, f) = (s, f12 . f . ft21)
          f21 (t, y) = (ft21 t, fy21 y)

parametriseMethodTransform :: (Method t1 y1 h1 -> Method t2 y2 h2) -> MethodWithParams p t1 y1 h1 -> MethodWithParams p t2 y2 h2
parametriseMethodTransform mt mwp p = mt $ mwp p
