module Scenario where

import Predictor

type DataLoader t y = String -> IO(Dataset t y)
type Scenario t y = (String, DataLoader t y, ErrorFunc y)

processScenario :: Scenario t y -> String -> [String] -> [Method t y] -> IO()
processScenario (name, load, errf) trn tsts methods = do
    putStrLn name;
    train <- load trn;
    let preds = map (\m -> m train) methods;
    putStrLn $ show $ map (\(s, _) -> s) preds;
    tests <- sequence $ map load tsts;
    let errors = map (\p -> map (\t -> meanError p t errf) tests) preds;
    putStrLn $ show errors;
    return ();

