module Predictor where

-- |A dataset consists of a header and a list of tuples of times and values.
type Dataset t y h = (h, [(t, y)])
-- |A predictor consists of its name and a function which assigns a value to a time.
type Predictor t y = (String, t -> y)
-- |Error function takes the predicted and actual value and returns a float.
type ErrorFunc y = y -> y -> Float
-- |Error evaluator evaluates the error of a predictor on given dataset
type ErrorEvaluator t y h = Predictor t y -> Dataset t y h -> Float

-- |A method is a function which trains a predictor on given training dataset.
type Method t y h = Dataset t y h -> Predictor t y
-- |A method with parameters is a method which must be given a parameter in advance.
type MethodWithParams p t y h = p -> Method t y h

-- |This error evaluator calculates the error at each data point and calculates the arithmetic mean.
meanError :: ErrorFunc y -> ErrorEvaluator t y h
meanError err (_, pred) (_, test) = (sum $ map (\(t_, y_) -> err (pred t_) y_) test) / (fromIntegral $ length test)

-- |This function transforms a method with parameters to a method without
-- parameters by offering a list of parameters. The result method tries all of
-- these parameters and chooses the best predictor.
bruteForceTrain :: MethodWithParams p t y h -> [p] -> ErrorEvaluator t y h -> Method t y h
bruteForceTrain mwp params err = bestm
    where bestm dat = best
              where preds = map (\p -> mwp p dat) params
                    errs = map (\pr -> err pr dat) preds
                    s = zip errs preds
                    (_, best) = foldr better (head s) (tail s)
                    better (e1, p1) (e2, p2) | e2 > e1 = (e1, p1)
                                             | otherwise = (e2, p2)

-- |Transforms the method so it will use different types
methodChangeType :: (t2 -> t1) -> (y2 -> y1) -> (y1 -> y2) -> (h2 -> h1) -> Method t1 y1 h1 -> Method t2 y2 h2
methodChangeType ft21 fy21 f12 fh m1 = \(header, dat) -> tt $ m1 $ (fh header, map f21 dat)
    where tt (s, f) = (s, f12 . f . ft21)
          f21 (t, y) = (ft21 t, fy21 y)

-- |Changes the header of used datasets
methodChangeHeader :: (h2 -> h1) -> Method t y h1 -> Method t y h2
methodChangeHeader = methodChangeType id id id

-- |Uses a dataset filter before the method is applied
methodFilterDataset :: (Dataset t y h2 -> Dataset t y h1) -> Method t y h1 -> Method t y h2
methodFilterDataset filter m dat = m $ filter dat

-- |This function transforms a method transformation so it will transform
-- methods with parameters in the same way.
parametriseMethodTransform :: (Method t1 y1 h1 -> Method t2 y2 h2) -> MethodWithParams p t1 y1 h1 -> MethodWithParams p t2 y2 h2
parametriseMethodTransform mt mwp p = mt $ mwp p

-- |This function does something similar to parametriseMethodTransform but this
-- time, the method transformation has an additional parameter. This parameter
-- is added to the resulting method with params.
parametriseMethodTransform' :: (p' -> Method t1 y1 h1 -> Method t2 y2 h2) -> MethodWithParams p t1 y1 h1 -> MethodWithParams (p', p) t2 y2 h2
parametriseMethodTransform' mt mwp (p', p) = mt p' $ mwp p

