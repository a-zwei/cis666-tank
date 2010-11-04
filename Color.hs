module Main where

import Data.Array.IArray
import Data.Function (on)
import Data.Graph.Inductive
import Data.List (sortBy)
import System.Random

data State = State { us :: Array (Int, Int) Float }
  deriving (Read, Show, Eq)

type UGraph = Gr () ()

randomArray :: (Ix i, Random a) => (i, i) -> (a, a) -> Int -> Array i a
randomArray bounds range seed = listArray bounds $ randomRs range g
  where g = mkStdGen seed

sampleA = State $ randomArray ((1,1),(6,3)) (-0.5, 0.5) 1

randomGraph :: Int -> Int -> Int -> UGraph
randomGraph nodes edges seed = undir $ mkUGraph [1..nodes] $ take edges rEdges
  where rEdges = filter (\(i,j) -> i /= j) $ zip rNodes (tail rNodes)
        rNodes = randomRs (1, nodes) (mkStdGen seed)

boolToInt True = 1
boolToInt False = 0

vs :: State -> Array (Int, Int) Int
vs s = amap (boolToInt . (>= 0)) (us s)

cv :: UGraph -> Node -> Node -> Int
cv g i j = boolToInt $ j `elem` neighbors g i

nodeBounds :: State -> (Int, Int)
nodeBounds s = (\((i,_),(j,_)) -> (i, j)) $ bounds (us s)

colorBounds :: State -> (Int, Int)
colorBounds s = (\((_,i),(_,j)) -> (i, j)) $ bounds (us s)

nodeIxs :: State -> [Int]
nodeIxs = range . nodeBounds

colorIxs :: State -> [Int]
colorIxs = range . colorBounds

nodeVs :: State -> Int -> [Int]
nodeVs s n = [vs s ! (n, c) | c <- colorIxs s]

colorVs s c = [vs s ! (n, c) | n <- nodeIxs s]

a = 10
b = 1
c = 20

dt = 0.01

dudt :: UGraph -> State -> (Int, Int) -> Float
dudt g s (node, color) = -(us s ! (node, color)) - fromIntegral
  (a * oneColorPerVertex + b * minimizeColor + c * noSameColorConnected)
  where oneColorPerVertex = sum (nodeVs s node) - 1
        minimizeColor = color - (fst $ colorBounds s)
        noSameColorConnected =
          sum [cv g node j * (vs s ! (j, color)) | j <- nodeIxs s] - 1

iterate :: UGraph -> State -> State
iterate g s = State $ array (bounds $ us s) (map updateU $ assocs (us s))
  where updateU ((i, j), u) = ((i, j), u + dt * dudt g s (i, j))
