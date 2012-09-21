{-# LANGUAGE NoMonomorphismRestriction, ForeignFunctionInterface #-}

module Merge where

import Data.Function
import Data.List
import Data.Array.IArray
import Data.Array.MArray
import Control.Monad
import Data.Array.ST
import Foreign.C.Types

type Point = (Double, Double)

floor' :: Double -> Int
floor' x = (truncate :: Double -> Int) (c_floor x)
{-# INLINE floor' #-}

foreign import ccall unsafe "math.h floor"
    c_floor :: Double -> Double

minimumBy' :: (a -> a -> Ordering) -> [a] -> a
minimumBy' cmp = foldl1' minBy where
  minBy x y = case cmp x y of
    GT -> y
    _  -> x

distance2 (x0,y0) (x1,y1) = (x1-x0)^2 + (y1-y0)^2

simpleFindBest :: Double -> [Point] -> Point -> Maybe Point
simpleFindBest eps2 ps p0 = 
  case filter ((<=eps2) . snd) [(p,distance2 p0 p) | p <- ps] of
    [] -> Nothing
    xs -> (Just . fst . minimumBy' (compare `on` snd)) xs

simpleMerge :: Double -> [Point] -> [Point] -> [(Point, Maybe Point)]
simpleMerge eps xs ys = [(x,g x) | x <- xs] where g = simpleFindBest (eps^2) ys

type Grid = Array Cell [Point]
type Cell = (Int,Int)

near :: Cell -> [Cell]
near (i,j) = [(i+x, j+y) | x <- [-1..1], y <- [-1..1]]

mkGrid :: Double -> (Point,Point) -> [Point] -> (Grid, Point -> Cell)
mkGrid d (p0,p1) ps = 
  let cell (x,y) = (g (fst p0) x, g (snd p0) y) where
        g a b = floor' $ (b-a) / d
      grid = runSTArray $ do 
        arr <- newArray (cell p0, cell p1) []
        forM_ ps $ \p -> do
          let c = cell p
          v <- readArray arr c
          writeArray arr c (p:v)
        return arr
  in (grid, cell)
        
gridMerge :: Double -> Double -> [Point] -> [Point] -> [(Point, Maybe Point)]
gridMerge eps diam xs ys = [(x, gridFindBest x) | x <- xs]
  where
    gridFindBest p = simpleFindBest eps2 ps p where
      ps = (concatMap (grid !) . near . cell) p 
    eps2 = eps^2
    (grid,cell) = mkGrid diam ((-1000,-1000), (5000,5000)) ys

minB = evB minimum
maxB = evB maximum

evB g = uncurry ((,) `on` g) . unzip

