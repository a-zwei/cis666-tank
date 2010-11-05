module GraphColor (Color, randomState, colorGraph) where

import Data.Array.Unboxed
import Data.List (delete, sortBy)
import System.Random

import UGraph
import Util

a = 10
b = 1
c = 20

dt = 0.01

type Color = Int -- Node is also Int (from Graph)

data State = State { us :: Array (Node, Color) Float }
  deriving (Read, Show, Eq)

vs :: State -> Array (Node, Color) Int
vs s = amap (boolToInt . (>= 0)) (us s)

nodeBounds :: State -> (Node, Node)
nodeBounds s = (\((i,_),(j,_)) -> (i, j)) $ bounds (us s)

colorBounds :: State -> (Color, Color)
colorBounds s = (\((_,i),(_,j)) -> (i, j)) $ bounds (us s)

nodeIxs :: State -> [Int]
nodeIxs = range . nodeBounds

colorIxs :: State -> [Int]
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

localMin :: [Float] -> Int
localMin (n:ns) = findLow' 0
  where findLow' ix = if (head ns) > n then ix else findLow' (ix + 1)

pickSolution :: UGraph -> [State] -> State
pickSolution g states = states !! localMin energies
  where energies = map (energy g) states

colorGraph :: UGraph -> Int -> Int -> [(Node, Color)]
colorGraph g colors seed =
  [(n, c) | n <- nodeIxs s, c <- colorIxs s, vs s ! (n, c) == 1]
  where s = pickSolution g $ iterate (update g) initialState
        initialState = randomState (length $ nodes g) colors seed
