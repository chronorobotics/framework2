module ResultCache where

import Data.Char
import Data.List (sortBy)
import System.Directory

type CacheEntry = (String, String, [Float])
type Cache = [CacheEntry]
type CacheOperation = (Bool, String, String, [Float])

nameToFile :: String -> String
nameToFile name = "results/"++(map (toLower . foo) name)++".txt"
    where foo ' ' = '_'
          foo x = x

spacesToUnderscores :: String -> String
spacesToUnderscores = map stu
    where stu ' ' = '_'
          stu x = x

underscoresToSpaces :: String -> String
underscoresToSpaces = map uts
    where uts '_' = ' '
          uts x = x

loadCache :: String -> IO(Cache)
loadCache name = do
    let fn = nameToFile name
    fe <- doesFileExist fn
    if fe then do
        content <- readFile fn
        let wss = map words $ lines content
        return $! map (\(w1:w2:ws) -> (w1, underscoresToSpaces w2, map read ws)) wss
    else do
        return []

saveCache :: String -> Cache -> IO()
saveCache name cache = writeFile (nameToFile name) $! foldr (j "\n") "" $ map ts cache
    where j b a c = a ++ b ++ c
          ts (s1, s2, fs) = s1 ++ " " ++ (spacesToUnderscores s2) ++ " " ++ (foldr (j " ") "" $ map show fs)

cacheChoose :: CacheEntry -> CacheOperation -> CacheEntry
cacheChoose (n, s, fs) (force, n', s', fs') | force = (n, s', fs')
                                            | otherwise = (n, s, fs)

cacheJoin :: Cache -> [CacheOperation] -> (Cache, Cache)
cacheJoin es' os' = (s1 r1, s1 r2)
    where es = s1 es'
          os = s2 os'
          s1 = sortBy (\(n, _, _) (n', _, _) -> compare n n')
          s2 = sortBy (\(_, n, _, _) (_, n', _, _) -> compare n n')
          df (_, n, s, fs) = (n, s, fs)
          rmf = map df
          m [] [] res1 res2 = (res1, res2)
          m ez [] res1 res2 = (ez ++ res1, res2)
          m [] oz res1 res2 = ((rmf oz) ++ res1, (rmf oz) ++ res2)
          m (e:ez) (o:oz) res1 res2 | cmp == LT = m ez (o:oz) (e:res1) res2
                                    | cmp == GT = m (e:ez) oz ((df o):res1) ((df o):res2)
                                    | f = m ez oz ((df o):res1) ((df o):res2)
                                    | otherwise = m ez oz (e:res1) (e:res2)
              where cmp = compare n n'
                    (n, _, _) = e
                    (f, n', _, _) = o
          (r1, r2) = m es os [] []

