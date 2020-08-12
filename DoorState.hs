import Predictor
import Scenario
import ErrorFunctions
import TimeValueLoader

import Mean
import Histogram

doorState :: Scenario Double Float ()
doorState = ("Door State Prediction", timeValueLoader, meanError squareError)

main :: IO()
main = processScenario doorState
       "greg_door_2016_min/training_data.txt"
       ([("greg_door_2016_min/test_all_0.txt", "Train.")] ++ map (\a -> ("greg_door_2016_min/test_all_"++(show a)++".txt", "Test "++(show a))) [1..9])
       [
           meanMethod,
           bruteForceTrain histogramMethod (map (\a -> (86400, a)) [3,4,6,12,24]) (meanError squareError)
       ]

