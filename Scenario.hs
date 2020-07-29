module Scenario where

import Predictor

type DataLoader t y = String -> IO(Dataset t y)
type Scenario t y = (String, DataLoader t y, ErrorEvaluator t y)

processScenario :: Scenario t y -> String -> [String] -> [Method t y] -> IO()
processScenario (name, load, erre) trn tsts methods = do
    putStrLn name;
    train <- load trn;
    let preds = map (\m -> m train) methods;
    sequence $ map putStrLn $ map (\(s, _) -> s) preds;
    tests <- sequence $ map load tsts;
    let errors = map (\p -> map (\t -> erre p t) tests) preds;
    sequence $ map (putStrLn . show) errors;
    return ();

