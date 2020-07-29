module PedestriansLoader where

import Scenario
import Cell

pedestriansLoader :: (Read x, RealFrac x, RealFrac y) => x -> x -> DataLoader (Cell x) y (CellGrid x)
pedestriansLoader temporalResolution spatialResolution filename = do
    content <- readFile filename
    let ls' = lines content
    let tsize = parseLnI ls' 0
    let xsize = parseLnI ls' 1
    let ysize = parseLnI ls' 2
    let (_, ls) = splitAt 3 ls'
    let grid = (([tsize !! 0, xsize !! 0, ysize !! 0], [sz tsize, sz xsize, sz ysize]), [temporalResolution, spatialResolution, spatialResolution])
    return (grid, map ((\xs -> ((xs, [0,0,0]), 1)) . parseLn) ls)
  where parseLn = (map read) . words :: (Read x, RealFrac x) => String -> [x]
        parseLnI ls_ i_ = parseLn (ls_ !! i_)
        sz arr = (arr !! 1) - (arr !! 0)

