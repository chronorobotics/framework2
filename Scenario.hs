module Scenario where

import Predictor
import Statistics.Test.StudentT
import Statistics.Types
import qualified Data.Vector
import System.Process
import System.Directory

type DataLoader t y h = String -> IO(Dataset t y h)
type Scenario t y h = (String, DataLoader t y h, ErrorEvaluator t y h)

isBetter :: [Double] -> [Double] -> Bool
isBetter as bs = isyes $ studentTTest BGreater (Data.Vector.fromList as) (Data.Vector.fromList bs)
    where isyes (Just t) = (Statistics.Types.pValue $ testSignificance t) < 0.05
          isyes Nothing = False

oneArrow :: (String, [Double]) -> (String, [Double]) -> String
oneArrow (an, ae) (bn, be) | isBetter ae be = "\""++an++"\" -> \""++bn++"\"\n"
                           | otherwise = ""

processScenario :: Scenario t y h -> String -> [(String, String)] -> [Method t y h] -> IO()
processScenario (name, load, erre) trn tsts methods = do
    putStrLn name;
    train <- load trn;
    let preds = map (\m -> m train) methods;
    let names = map (\(s, _) -> s) preds;
    sequence $ map (putStrLn . show) names;
    tests <- sequence $ map (\(f, n) -> load f) tsts;
    let errors = map (\p -> map (\t -> erre p t) tests) preds;
    sequence $ map (putStrLn . show) errors;
    writeFile "tmp/results.py" $ "name=\""++name++"\"\nmethods="++(show names)++"\nerrors="++(show errors)++"\nlabels="++(show $ map (\(f, n) -> n) tsts);
    let nes = zip names (map (map realToFrac) errors);
    let dot = foldr (++) "" $ map (\ne -> foldr (++) "" $ map (oneArrow ne) nes) nes;
    let dot' = foldr (++) "" $ map (\n -> "\""++n++"\"\n") names;
    writeFile "tmp/graph.dot" $ "digraph\n{\nnode [penwidth=2 fontname=\"palatino bold\"]\nedge [penwidth=2]\n"++dot++dot'++"}";
    cwd <- getCurrentDirectory;
    readProcess "bash" [cwd++"/summary.sh", name] "";
    return ();

