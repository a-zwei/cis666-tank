module Main where

import Data.Array.IArray
import Data.Function (on)
import Data.Graph
import Data.List (sortBy)

data Item = Item { name :: String, weight :: Float, value :: Float }
  deriving (Read, Show, Eq)

data State = State { us :: Array (Int, Int) Float }
  deriving (Read, Show, Eq)

vs :: State -> Array (Int, Int) Bool
vs s = amap (>= 0) (us s)

rowBounds :: State -> (Int, Int)
rowBounds s = (\((r0,_),(r1,_)) -> (r0, r1)) $ bounds (us s)

colBounds :: State -> (Int, Int)
colBounds s = (\((_,c0),(_,c1)) -> (c0, c1)) $ bounds (us s)

rows :: State -> [Int]
rows = range . rowBounds

cols :: State -> [Int]
cols = range . colBounds

--dudt :: State -> (Int, Int) -> Int
--dudt s (i, j) = 

--iterate :: State -> State
--iterate s = Map.mapWithKey f 

maxItems capacity items = floor (capacity / leastWeight)
  where leastWeight = weight . head $ sortBy (compare `on` weight) items

bitsForMax n = ceiling (log (n + 1) / log 2)
