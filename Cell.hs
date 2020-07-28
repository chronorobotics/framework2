module Cell where

type Cell x = ([x], [x])

cellCentre :: RealFrac x => Cell x -> [x]
cellCentre (pos, size) = map (\(p,s) -> p+s/2) (zip pos size)

cellsIn :: (RealFrac x, Integral n) => x -> x -> n
cellsIn size_total size_subcell = -floor (-size_total / size_subcell)

subcellCount :: (RealFrac x, Integral n) => Cell x -> [x] -> n
subcellCount (pos, size_total) size_subcell = product $ map (\(st, sc) -> cellsIn st sc) (zip size_total size_subcell)

getSubcell :: (RealFrac x) => Cell x -> [x] -> [x] -> Cell x
getSubcell (cell_pos, _) size_subcell my_pos = (map (\(a,b) -> a+b) (zip cell_pos rpos), size_subcell)
    where my_pos' = map (\(a,b) -> a-b) (zip my_pos cell_pos)
          rpos = map (\(sc, mp) -> (fromIntegral $ floor (mp/sc))*sc) (zip size_subcell my_pos')

getSubcellId :: (RealFrac x, Integral n) => Cell x -> [x] -> [x] -> n
getSubcellId (cell_pos, size_total) size_subcell my_pos = foldr (\(cc, cp) r -> cp+cc*r) 0 foo
    where sc_counts = map (\(sc, st) -> floor(st/sc)) (zip size_subcell size_total)
          my_pos' = map (\(a,b) -> a-b) (zip my_pos cell_pos)
          my_pos'' = map (\(sc, mp) -> floor(mp/sc)) (zip size_subcell my_pos')
          foo = zip sc_counts my_pos''

getSubcellById :: (RealFrac x, Integral n) => Cell x -> [x] -> n -> Cell x
getSubcellById (cell_pos, size_total) size_subcell my_id = (map (\((a,b),c) -> (fromIntegral a)*b + c) (zip (zip log_pos size_subcell) cell_pos), size_subcell)
    where sc_counts = map (\(sc, st) -> floor(st/sc)) (zip size_subcell size_total)
          (log_pos, _) = foldl (\(lp, i) scc -> (lp ++ [i `mod` scc], i `div` scc)) ([], my_id) sc_counts

