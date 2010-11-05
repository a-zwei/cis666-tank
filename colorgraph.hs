module Main where

import System.Environment
import System.Random

import GraphColor
import UGraph

fromJust (Just a) = a

colorNames = ["chocolate", "crimson", "cornflowerblue", "darkgreen",
          "peachpuff", "goldenrod", "orange", "deeppink", "indigo", "olivedrab",
          "palegreen", "orchid", "palegreen", "royalblue", "slategrey",
          "yellow", "wheat", "tan", "thistle", "tomato", "turquoise"]

neato :: UGraph -> [(Node, Color)] -> String
neato g colors = "graph {" ++ concat nodeColors ++ concat edgeLines ++ "\n}"
  where edgeLines = map fmtEdges $ unique (edges g)
        fmtEdges (i, j) = "\n  " ++ show i ++ " -- " ++ show j
        nodeColors = map fmtColors $ nodes g
        fmtColors n = "\n" ++ show n ++  " [style=filled fillcolor=\"" ++
          (colorNames !! (fromJust $ lookup n colors)) ++ "\"]"

main = do
  [graphFile] <- getArgs
  g <- loadGraph graphFile
  seed <- randomIO
  putStrLn $ neato g (colorGraph g (noNodes g) seed)
