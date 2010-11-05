module GraphColor (Color, colorGraph, nodeColor, randomState, State) where

import Data.Array.Unboxed
import Data.List (delete, sortBy)
import System.Random

import UGraph
import Util

a = 250 -- one color per vertex
b = 100 -- minimize color index
c = 250 -- neighbors different colors

dt = 0.075

type Color = Int -- Node is also Int (from Graph)

data State = State { us :: Array (Node, Color) Float }
  deriving (Read, Show, Eq)

vs :: State -> Array (Node, Color) Int
vs s = amap (boolToInt . (>= 0)) (us s)

nodeBounds :: State -> (Node, Node)
nodeBounds s = (\((i,_),(j,_)) -> (i, j)) $ bounds (us s)

colorBounds :: State -> (Color, Color)
colorBounds s = (\((_,i),(_,j)) -> (i, j)) $ bounds (us s)

nodeIxs :: State -> [Node]
nodeIxs = range . nodeBounds

colorIxs :: State -> [Color]
colorIxs = range . colorBounds

nodeVs :: State -> Int -> [Int]
nodeVs s n = [vs s ! (n, c) | c <- colorIxs s]

colorVs s c = [vs s ! (n, c) | n <- nodeIxs s]

dudt :: UGraph -> State -> (Int, Int) -> Float
dudt g s (node, color) = -(us s ! (node, color)) - fromIntegral
  (a * oneColorPerVertex + b * minimizeColor + c * noSameColorConnected)
  where oneColorPerVertex = sum (nodeVs s node) - 1
        minimizeColor = color - (fst $ colorBounds s)
        noSameColorConnected =
          sum [cv g (node, j) * (vs s ! (j, color)) | j <- nodeIxs s] - 1

energy :: UGraph -> State -> Float
energy g s = fromIntegral $ a * aTerm + b * bTerm + c * cTerm
  where aTerm = sum [sum [(vs s ! (i, k)) * (vs s ! (i, j)) |
          k <- delete j $ colorIxs s] + (vs s ! (i, j)) |
            i <- nodeIxs s, j <- colorIxs s]
        bTerm = sum [(j + 1) * (vs s ! (i, j)) |
          i <- nodeIxs s, j <- colorIxs s]
        cTerm = sum [sum [cv g (i, k) * (vs s ! (k, j)) * (vs s ! (i, j)) |
          k <- delete i $ nodeIxs s] + (vs s ! (i, j)) |
            i <- nodeIxs s, j <- colorIxs s]

update :: UGraph -> State -> State
update g s = State $ array (bounds $ us s) (map updateU $ assocs (us s))
  where updateU ((i, j), u) = ((i, j), u + dt * dudt g s (i, j))

randomArray :: (Ix i, Random a) => (i, i) -> (a, a) -> Int -> Array i a
randomArray bounds range seed = listArray bounds $ randomRs range g
  where g = mkStdGen seed

randomState :: Int -> Int -> Int -> State
randomState nodes colors seed =
  State $ randomArray ((1, 1), (nodes, colors)) (-0.5, 0.5) seed

nodeColors :: State -> [(Node, Color)]
nodeColors s = [(n, c) | n <- nodeIxs s, c <- colorIxs s, vs s ! (n, c) == 1]

nodeColor :: State -> Node -> Color
nodeColor s n = fromJust (lookup n $ nodeColors s)

allColored :: State -> Bool
allColored s = all (/= Nothing) [lookup n $ nodeColors s | n <- nodeIxs s]

neighborColors :: UGraph -> State -> Node -> [Color]
neighborColors g s node = [nodeColor s n | n <- neighbors g node]

neighborsDiffer :: UGraph -> State -> Bool
neighborsDiffer g s = not $ or [nodeColor s n `elem` neighborColors g s n |
  n <- nodes g]

localMin :: Ord a => [a] -> Int
localMin ns = f ns 0
  where f (n:ns) ix = if (head ns) > n then ix else f ns (ix + 1)

nextMin :: Ord a => Int -> [a] -> Int
nextMin ix ns = 1 + ix + (localMin $ drop (ix + 1) ns)

colorGraph :: UGraph -> Int -> State
colorGraph g seed = states !! localMin energies
  where initialState = randomState (noNodes g) (noNodes g) seed
        states = filter valid $ iterate (update g) initialState
        energies = map (energy g) states
        valid s = allColored s && neighborsDiffer g s
