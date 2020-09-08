{-# LANGUAGE FlexibleContexts #-}
import Predictor
import Scenario
import ErrorFunctions
import TimeValueLoader

import Mean
import Histogram
import FreMEn

import Trimmer

import DistributionMethod
import EMAlgorithm
import WrappedTime
import WrappedCauchy
import VonMises

doorState :: Scenario Double Float ()
doorState = ("Door State Prediction", timeValueLoader, meanError squareError)

bdm :: (EMDistribution d Double) => () -> DistributionToMethod Double Float () [(Double, d)]
bdm = const binaryDistributionMethod

main :: IO()
main = processScenario doorState
       "greg_door_2016_min/training_data.txt"
       ([("greg_door_2016_min/test_all_0.txt", "Train.")] ++ map (\a -> ("greg_door_2016_min/test_all_"++(show a)++".txt", "Test "++(show a))) [1..9])
       ( []
       >>! "Mean" :> meanMethod
       >>! "Hist" :> bruteForceTrain histogramMethod (map (\a -> (86400, a)) [3,4,6,12,24]) (meanError squareError)
       >>! "FreMEn" :> bruteForceTrain (parametriseMethodTransform (trimmer 0 1) freMEn) (map (\a -> (604800, 84, a)) [1..5]) (meanError squareError)
       >>! "EM-wC" :> bruteForceTrain (parametriseMethodTransform' (wrappedMethod wrapTime') $ createDistributionMethod bdm emAlgorithm) (map (\a -> (604800, ((), (a, 7350, initWrappedCauchy 0.999, 1.0, wrappedCauchyMaxMu 0.999)))) [5,10,15,20,25,30]) (meanError squareError)
       >>! "EM-vM" :> bruteForceTrain (parametriseMethodTransform' (wrappedMethod wrapTime') $ createDistributionMethod bdm emAlgorithm) (map (\a -> (604800, ((), (a, 7351, initVonMises 100, 1.0, vonMisesMaxKappa 300)))) [5,10,15,20,25,30]) (meanError squareError)
       )

