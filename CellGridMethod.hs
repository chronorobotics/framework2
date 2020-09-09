module CellGridMethod where

import Predictor
import Cell
import Data.Vector ((!), fromList)

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
                    preds' = map (\i -> m (hf rest i, map (\(_, t, y) -> (t, y)) $ filter (\(c, t, y) -> c == i) dat')) [0..((subcellCount grid) - 1)]
                    dat' = map (\((t, x), y) -> (getSubcellId grid x, t, y)) dat
                    (name, _) = preds' !! 0
                    preds = Data.Vector.fromList $ map snd $! preds'

-- |This function does the same as cellGridMethod but it applies to a method
--  with parameters. The same parameters are being passed to each cell.
cellGridMethodParams :: RealFrac x => (h -> Int -> h') -> MethodWithParams p t y h' -> MethodWithParams p (t, [x]) y (CellGrid x, h)
cellGridMethodParams hf mwp = parametriseMethodTransform (cellGridMethod hf) mwp

