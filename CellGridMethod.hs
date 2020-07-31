module CellGridMethod where

import Predictor
import Cell
import Data.Sequence

cellGridMethod :: RealFrac x => (h -> Int -> h') -> Method t y h' -> Method (t, [x]) y (CellGrid x, h)
cellGridMethod hf m = cgm
    where cgm ((grid, rest), dat) = (name, pred)
              where cc = subcellCount grid
                    pred (t, x) = (index preds (getSubcellId grid x)) t
                    preds' = fmap (\i -> m (hf rest i, index dat' i)) $ fromList [0..((subcellCount grid) - 1)]
                    dat0 = Data.Sequence.replicate cc []
                    add ((t, x), y) d = update i ((t, y) : (index d i)) d
                        where i = getSubcellId grid x
                    dat' = foldr add dat0 dat
                    (name, _) = index preds' 0
                    preds = fmap (\(_, p) -> p) preds'

cellGridMethodParams :: RealFrac x => (h -> Int -> h') -> MethodWithParams p t y h' -> MethodWithParams p (t, [x]) y (CellGrid x, h)
cellGridMethodParams hf mwp = parametriseMethodTransform (cellGridMethod hf) mwp

