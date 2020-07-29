module TimeValueLoader where

import Scenario

timeValueLoader :: (Read t, Read v) => DataLoader t v ()
timeValueLoader filename = do
    content <- readFile filename;
    let ls = lines content
    return ((), map ((\[t,v] -> (read t, read v)) . words) ls);
