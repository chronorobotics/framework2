module Hypertime where

import Predictor
import FreMEn
import WrappedTime

hypertime :: RealFloat t => MethodWithParams p [t] y h -> MethodWithParams ((t, Int, ErrorFunc y), Int -> p) ([t], t) y h
hypertime method ((maxPeriod, elements, err), param) (h, dat) = ("HyT-"++name, best)
    where space dat' = map (\((x, _), y) -> (x, y)) dat'
          pred0 = method (param 0) (h, space dat)
          errorOf pred dat' = (meanError err) pred (h, space dat')
          nextPeriod (_, pred) dat' = period $ head $ snd $ trainFrelements (maxPeriod, elements, 1) (map (\((x, t), y) -> (t, realToFrac $ err (pred x) y)) dat')
          addPeriod p dat' = map (\((x, t), y) -> (([cos $ 2*pi*t/p, sin $ 2*pi*t/p] ++ x, t), y)) dat'
          hypertime' pred error dat' ps i | error > error' = hypertime' pred' error' dat'' (np:ps) (i+1)
                                          | otherwise = (pred, ps)
              where np = nextPeriod pred dat'
                    dat'' = addPeriod np dat'
                    pred' = method (param (i+1)) (h, space dat'')
                    error' = errorOf pred' dat''
          ((name, best'), periods) = hypertime' pred0 (errorOf pred0 dat) dat [] 0
          best (s, t) = best' $ (wrapHyperTime periods t) ++ s

