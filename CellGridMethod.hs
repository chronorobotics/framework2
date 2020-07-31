module CellGridMethod where

import Predictor
import Cell
import Data.Sequence
import Data.Vector

-- |This function transforms a method to a method with larger input dimension so
-- the input space is split into cells and the given method is used for each
-- cell independently. The header of dataset is modified by a function which is
-- given a part of the original header and the index of cell which the method is
-- applied at.
cellGridMethod :: RealFrac x => (h -> Int -> h') -> Method t y h' -> Method (t, [x]) y (CellGrid x, h)
cellGridMethod hf m = cgm
    where cgm ((grid, rest), dat) = (name, pred)
              where cc = subcellCount grid
                    pred (t, x) = (preds ! (getSubcellId grid x)) t
                    preds' = Prelude.map (\i -> m (hf rest i, index dat' i)) [0..((subcellCount grid) - 1)]
                    dat0 = Data.Sequence.replicate cc []
                    add ((t, x), y) d = Data.Sequence.update i ((t, y) : (index d i)) d
                        where i = getSubcellId grid x
                    dat' = Prelude.foldr add dat0 dat
                    (name, _) = preds' !! 0
                    preds = Data.Vector.fromList $ Prelude.map (\(_, p) -> p) preds'

-- |This function does the same as cellGridMethod but it applies to a method
--  with parameters. The same parameters are being passed to each cell.
cellGridMethodParams :: RealFrac x => (h -> Int -> h') -> MethodWithParams p t y h' -> MethodWithParams p (t, [x]) y (CellGrid x, h)
cellGridMethodParams hf mwp = parametriseMethodTransform (cellGridMethod hf) mwp

