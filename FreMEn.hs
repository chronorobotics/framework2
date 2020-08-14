module FreMEn where

import Predictor
import Data.List (sortBy)
import Debug.Trace

data Frelement f = Frelement {amplitude :: f, phase :: f, period :: f} deriving (Eq, Show)

trainFrelements :: (RealFloat f, RealFloat t) => (t, Int, Int) -> [(f, f)] -> (f, [Frelement f])
trainFrelements (maxPeriod, elements, order) dat = (storedGain, frelements)
    where storedGain = (sum $ map (\(_, y) -> y) dat) / (fromIntegral $ length dat)
          periods = map (realToFrac . (maxPeriod/) . fromIntegral) [1..elements]
          angle t p = 2*pi*t / p
          stored fy fa = map (\p -> sum $ map (\(t, y) -> (fy y)*(fa $ angle t p)) dat) periods
          stored' fa = map (\(a,b) -> a - storedGain*b) $ zip (stored id fa) (stored (const 1) fa)
          states = zip (stored' cos) (stored' sin)
          amplitudes = map (\(re,im) -> 2*(sqrt (re*re + im*im)) / (fromIntegral $ length dat)) states
          phases = map (\(re,im) -> atan2 im re) states
          frelements' = take order $ sortBy (\(a1,_) (a2,_) -> compare a2 a1) $ zip amplitudes $ zip phases periods
          frelements = map (\(a,(phi,p)) -> Frelement{amplitude = a, phase = phi, period = p}) frelements'

evalFrelement :: Floating f => f -> Frelement f -> f
evalFrelement t Frelement{amplitude = a, phase = phi, period = p} = a * (cos $ 2*pi*t/p - phi)

freMEn :: (RealFloat t, RealFrac y) => MethodWithParams (t, Int, Int) t y h
freMEn (maxPeriod, elements, order) (_, dat) = ("FreMen "++(show order), predict)
    where dat' = map (\(t,y) -> (realToFrac t, realToFrac y)) dat :: [(Double, Double)]
          (storedGain, frelements) = trainFrelements (maxPeriod, elements, order) dat'
          predict time = realToFrac $ storedGain + (sum $ map (evalFrelement $ realToFrac time) frelements)

