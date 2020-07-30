import Predictor
import Scenario
import ErrorFunctions
import PedestriansLoader

import Mean
import Histogram

import Cell
import CellGridMethod
import DataCollector
import PedestriansErrorEvaluator

pedestriansTransform :: Method Double Float () -> Method (Cell Double) Float (CellGrid Double)
pedestriansTransform = (methodChangeType sdt id id sdh) . (cellGridMethod (\h _ -> (h, ()))) . (methodFilterDataset $ dataCollector 0 (+)) . (methodChangeType (\[t] -> t) id id id)
    where sdh (([t0,x0,y0], [ts,xs,ys]), [t,x,y]) = ((([x0,y0], [xs,ys]), [x,y]), (([t0], [ts]), [t]))
          sdt ([t, x, y], _) = ([t], [x, y])

pedestriansTransform' :: MethodWithParams p Double Float () -> MethodWithParams p (Cell Double) Float (CellGrid Double)
pedestriansTransform' = parametriseMethodTransform pedestriansTransform

spatialResolution = 0.5
temporalResolution = 900

pedestrians :: Scenario (Cell Double) Float (CellGrid Double)
pedestrians = ("Pedestrians Prediction", pedestriansLoader temporalResolution spatialResolution, pedestriansErrorEvaluator squareError)

main :: IO ()
main = processScenario pedestrians
       "lincoln/trenovaci_dva_tydny.txt"
       ["lincoln/testovaci_dva_dny.txt", "lincoln/testovaci_tri_dny.txt", "lincoln/kontrolni_ctvrtek.txt"]
       [
           pedestriansTransform meanMethod,
           bruteForceTrain (pedestriansTransform' histogramMethod) (map (\a -> (86400, a)) [3,4,6,12,24]) (pedestriansErrorEvaluator squareError)
       ]
