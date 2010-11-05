module Main where

import System.Environment
import System.Random

import GraphColor
import UGraph

colorNames = ["chocolate", "crimson", "cornflowerblue", "darkgreen",
          "peachpuff", "goldenrod", "orange", "deeppink", "indigo", "olivedrab",
          "palegreen", "orchid", "palegreen", "royalblue", "slategrey",
          "yellow", "wheat", "tan", "thistle", "tomato", "turquoise"]

neato :: UGraph -> State -> String
neato g s = "graph {" ++ concat nodeColors ++ concat edgeLines ++ "\n}"
  where edgeLines = map fmtEdge $ unique (edges g)
        nodeColors = map fmtColor $ nodes g
        fmtEdge (i, j) = "\n  " ++ show i ++ " -- " ++ show j
        fmtColor n = "\n" ++ show n ++  " [style=filled fillcolor=\"" ++
          (colorNames !! nodeColor s n ) ++ "\"]"

main = do
  [graphFile] <- getArgs
  g <- loadGraph graphFile
  seed <- randomIO
  putStrLn $ neato g (colorGraph g seed)
