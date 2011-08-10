module Vast.MapReduce ( reduce
                      , mapReduce ) where

import Control.Parallel

parallel :: Int -> [a] -> [a]
parallel n (x:xs) = x `par` (parallel (n-1) xs `pseq` (x:xs))
parallel 0 xs = xs
parallel _ [] = []

reduce :: Int -> [a] -> [a]
reduce _ [] = []
reduce n xs = let (ys, zs) = splitAt n xs
              in parallel n ys ++ reduce n zs
                 
mapReduce :: Int -> (a -> b) -> [a] -> [b]
mapReduce n f = reduce n . map f
