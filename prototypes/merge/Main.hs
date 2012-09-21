{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Data.Function
import Parser
import Merge
import Control.Monad
import Text.Printf
import System.Environment

pR :: [(Point,Point)] -> IO ()
pR ps = forM_ (zip [(1 :: Int)..] ps) $ \(i,((x0,y0),(x1,y1))) -> printf "%d (%f,%f) (%f,%f)\n" i x0 y0 x1 y1

main = do
--  diam <- fmap (read . (!! 0)) getArgs
  f1 <- fParse "kdata/image_001.txt"
  f2 <- fParse "kdata/image_002.txt"
  let ps = map (\(x,Just y) -> (x,y)) $ 
           filter ((\x -> case x of
                       Nothing -> False
                       Just _ -> True) . snd) $ 
           simpleMerge 10 f1 f2
  pR ps