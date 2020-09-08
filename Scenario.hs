module Scenario where

import Predictor
import ResultCache
import Statistics.Distribution
import Statistics.Distribution.StudentT
--import Statistics.Test.StudentT
--import Statistics.Types
--import qualified Data.Vector
import System.Process
import System.Directory

import Debug.Trace

mtrace a = trace (show a) a

type DataLoader t y h = String -> IO(Dataset t y h)
type Scenario t y h = (String, DataLoader t y h, ErrorEvaluator t y h)
type MethodEntry t y h = (Bool, String, Method t y h)

infixl 0 >>$
infixl 0 >>!
infixl 0 >>#

data MethodEntry' t y h = String :> Method t y h

(>>$) :: [MethodEntry t y h] -> MethodEntry' t y h -> [MethodEntry t y h]
mes >>$ (name :> m) = (True, name, m):mes

(>>!) :: [MethodEntry t y h] -> MethodEntry' t y h -> [MethodEntry t y h]
mes >>! (name :> m) = (False, name, m):mes

(>>#) :: [MethodEntry t y h] -> MethodEntry' t y h -> [MethodEntry t y h]
mes >># (name :> m) = mes

isBetter :: [Double] -> [Double] -> Bool
isBetter as bs | n < 2 || as == bs = False
               | otherwise = q > 0.95 -- workaround due to a bug with Data.Vector
    where d = zipWith (-) bs as
          d_sum = sum d
          n = fromIntegral $ length as
          df = n - 1
          t = d_sum / ((n * (sum $ map (^2) d) - d_sum^2) / df)
          q = cumulative (studentT df) t

oneArrow :: (String, [Double]) -> (String, [Double]) -> String
oneArrow (an, ae) (bn, be) | isBetter ae be = "\""++an++"\" -> \""++bn++"\"\n"
                           | otherwise = ""

doMethodEntry :: ErrorEvaluator t y h -> Dataset t y h -> [Dataset t y h] -> MethodEntry t y h -> CacheOperation
doMethodEntry erre trn tsts (force, name, method) = (force, name, str, map (erre (str, pred)) tsts)
    where (str, pred) = method trn

processScenario :: (Show t, Show y) => Scenario t y h -> String -> [(String, String)] -> [MethodEntry t y h] -> IO()
processScenario (name, load, erre) trn tsts mes = do
{-    let methods = map (\(_, _, m) -> m) mes
    putStrLn name;
    train <- load trn;
    let preds = map (\m -> m train) methods;
    let names = map (\(s, _) -> s) preds;
--    sequence $ map (putStrLn . show) names;
    tests <- sequence $ map (\(f, n) -> load f) tsts;
    let errors = map (\p -> map (\t -> erre p t) tests) preds;-}
    cache <- loadCache name;
    train <- load trn;
    tests <- sequence $ map (\(f, n) -> load f) tsts;
    let results' = map (doMethodEntry erre train tests) mes;
    let (cache', results) = cacheJoin cache results';
    let (names, errors) = unzip $ map (\(_, n, es) -> (n, es)) results;
    saveCache name cache';
    
    sequence $ map (putStrLn . show) names;
    sequence $ map (putStrLn . show) errors;
    
    writeFile "tmp/results.py" $ "name=\""++name++"\"\nmethods="++(show names)++"\nerrors="++(show errors)++"\nlabels="++(show $ map (\(f, n) -> n) tsts);
    let nes = zip names (map (map realToFrac) errors);
    let dot = foldr (++) "" $ map (\ne -> foldr (++) "" $ map (oneArrow ne) nes) nes;
    let dot' = foldr (++) "" $ map (\n -> "\""++n++"\"\n") names;
    writeFile "tmp/graph.dot" $ "digraph\n{\nnode [penwidth=2 fontname=\"palatino bold\"]\nedge [penwidth=2]\n"++dot++dot'++"}";
    cwd <- getCurrentDirectory;
    readProcess "bash" [cwd++"/summary.sh", name] "";
    return ();


