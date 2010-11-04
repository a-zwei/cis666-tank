module Main where

import Data.Array.IArray
import Data.Function (on)
import Data.Graph
import Data.List (sortBy)
import System.Random

data State = State { us :: Array (Int, Int) Float }
  deriving (Read, Show, Eq)

randomArray :: (Ix i, Random a) => (i, i) -> (a, a) -> Int -> Array i a
randomArray bounds range seed = listArray bounds $ randomRs range g
  where g = mkStdGen seed

sampleA = State $ randomArray ((1,1),(6,3)) (-0.5, 0.5) 1

boolToInt True = 1
boolToInt False = 0

vs :: State -> Array (Int, Int) Int
vs s = amap (boolToInt . (>= 0)) (us s)

connected :: Graph -> Vertex -> Vertex -> Bool
connected _ i j | i == j = False
connected g i j = ((i, j) `elem` gvs) || ((j, i) `elem` gvs)
  where gvs = edges g

cv :: Graph -> Vertex -> Vertex -> Int
cv g i j = boolToInt $ connected g i j

rowBounds :: State -> (Int, Int)
rowBounds s = (\((i,_),(j,_)) -> (i, j)) $ bounds (us s)

colBounds :: State -> (Int, Int)
colBounds s = (\((_,i),(_,j)) -> (i, j)) $ bounds (us s)

rowIxs :: State -> [Int]
rowIxs = range . rowBounds

colIxs :: State -> [Int]
colIxs = range . colBounds

vertexVs :: State -> Int -> [Int]
vertexVs s v = [vs s ! (v, c) | c <- colIxs s]

colorVs s c = [vs s ! (v, c) | v <- rowIxs s]

a = 10
b = 1
c = 20

dudt :: Graph -> State -> (Int, Int) -> Float
dudt g s (vertex, color) = -(us s ! (vertex, color)) - fromIntegral
  (a * oneColorPerVertex + b * minimizeColor + c * noSameColorConnected)
  where oneColorPerVertex = sum (vertexVs s vertex) - 1
        minimizeColor = color - (fst $ colBounds s)
        noSameColorConnected =
          sum [cv g vertex j * (vs s ! (j, color)) | j <- rowIxs s] - 1

--iterate :: State -> State
--iterate s = Map.mapWithKey f 
