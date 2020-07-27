module ErrorFunctions where

import Predictor

absoluteError :: ErrorFunc Float
absoluteError a b = abs (a - b)

squareError :: ErrorFunc Float
squareError a b = (a - b)^2

squareDistance :: ErrorFunc [Float]
squareDistance a b = sum $ map (\(x,y) -> squareError x y) (zip a b)

eucleidanDistance :: ErrorFunc [Float]
eucleidanDistance a b = sqrt $ squareDistance a b

