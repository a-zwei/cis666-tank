module Main where

import System.Environment
import System.Random

import GraphColor
import UGraph

neato :: UGraph -> [(Node, Color)] -> String
neato g colors = "graph {\n" ++ concat edgeLines ++ "}"
  where edgeLines = map format $ unique (edges g)
        format (i, j) = "  " ++ show i ++
          " [label=\"color" ++ show (lookup i colors) ++ "\"] -- " ++ show j

main = do
  [graphFile] <- getArgs
  g <- loadGraph graphFile
  seed <- randomIO
  putStrLn $ neato g (colorGraph g (2 * length (edges g)) seed)
