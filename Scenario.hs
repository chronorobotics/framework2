module Scenario where

import Predictor

type DataLoader t y h = String -> IO(Dataset t y h)
type Scenario t y h = (String, DataLoader t y h, ErrorEvaluator t y h)

processScenario :: Scenario t y h -> String -> [String] -> [Method t y h] -> IO()
processScenario (name, load, erre) trn tsts methods = do
    putStrLn name;
    train <- load trn;
    let preds = map (\m -> m train) methods;
    sequence $ map putStrLn $ map (\(s, _) -> s) preds;
    tests <- sequence $ map load tsts;
    let errors = map (\p -> map (\t -> erre p t) tests) preds;
    sequence $ map (putStrLn . show) errors;
    return ();

