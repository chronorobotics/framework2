module Hypertime where

import Predictor
import FreMEn
import WrappedTime

import Debug.Trace
tr x = trace (show x) x

hypertime :: (RealFloat t, Show t, RealFloat k) => MethodWithParams p [t] y h -> MethodWithParams ((t, Int, ErrorEvaluator [t] y h, y -> y -> k), Int -> p) ([t], t) y h
hypertime method ((maxPeriod, elements, err, err2), param) (h, dat) = ("HyT-"++name, best)
    where space dat' = map (\((x, _), y) -> (x, y)) dat'
          pred0 = method (param 0) (h, space dat)
          errorOf pred dat' = err pred (h, space dat')
          nextPeriod (_, pred) dat' periods = next_p periods frelements
              where frelements = map period $ snd $ trainFrelements (maxPeriod, elements, elements) (map (\((x, t), y) -> (t, realToFrac $ err2 (pred x) y)) dat')
                    next_p ps (f:fs) | f `elem` ps = next_p ps fs
                                     | otherwise = f
          addPeriod p dat' = map (\((x, t), y) -> (([cos $ 2*pi*t/p, sin $ 2*pi*t/p] ++ x, t), y)) dat'
          hypertime' pred error dat' ps i | error > error' = hypertime' pred' error' dat'' (np:ps) (i+1)
                                          | otherwise = (pred, ps)
              where np = tr $ nextPeriod pred dat' ps
                    dat'' = addPeriod np dat'
                    pred' = method (param (i+1)) (h, space dat'')
                    error' = tr $ errorOf pred' dat''
          ((name, best'), periods) = hypertime' pred0 (errorOf pred0 dat) dat [] 0
          best (s, t) = best' $ (wrapHyperTime periods t) ++ s

