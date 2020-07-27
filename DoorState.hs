import Predictor
import Scenario
import ErrorFunctions
import TimeValueLoader

import Mean
import Histogram

doorState :: Scenario Double Float
doorState = ("Door State Prediction", timeValueLoader, squareError)

main :: IO()
main = processScenario doorState
       "greg_door_2016_min/training_data.txt"
       (map (\a -> "greg_door_2016_min/test_all_"++(show a)++".txt") [0..9])
       [
           meanMethod,
           bruteForceTrain histogramMethod (map (\a -> (86400, a)) [3,4,6,12,24]) squareError
       ]

