module CellGridMethod where

import Predictor
import Cell

cellGridMethod :: (RealFrac x, Integral n) => (h -> n -> h') -> Method t y h' -> Method (t, [x]) y (CellGrid x, h)
cellGridMethod hf m = cgm
    where cgm ((grid, rest), dat) = (name, pred)
              where pred (t, x) = (preds !! (getSubcellId grid x)) t
                    preds' = map (\i -> m (hf rest i, map (\((t, c), y) -> (t, y)) $ filter (\((t, c), y) -> c == i) dat_wcellid)) [0..((subcellCount grid) - 1)]
                    dat_wcellid = map (\((t, x), y) -> ((t, getSubcellId grid x), y)) dat
                    (name, _) = preds' !! 0
                    preds = map (\(_, p) -> p) preds'

cellGridMethodParams :: (RealFrac x, Integral n) => (h -> n -> h') -> MethodWithParams p t y h' -> MethodWithParams p (t, [x]) y (CellGrid x, h)
cellGridMethodParams hf mwp = parametriseMethodTransform (cellGridMethod hf) mwp

