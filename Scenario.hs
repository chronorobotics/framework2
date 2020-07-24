module Scenario where

import Predictor

type Scenario t y = (String, String -> IO(Dataset t y))

processScenario :: Scenario t y -> String -> [String] -> [Method t y] -> ErrorFunc y -> IO()
processScenario (name, load) trn tsts methods errf = do
    putStrLn name;
    train <- load trn;
    let preds = map (\m -> m train) methods;
    putStrLn $ show $ map (\(s, _) -> s) preds;
    tests <- sequence $ map load tsts;
    let errors = map (\p -> map (\t -> meanError p t errf) tests) preds;
    putStrLn $ show errors;
    return ();

