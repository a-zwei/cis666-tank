module Main where

import System.Environment
import System.Random

import UGraph

main = do
  [sNodes, sEdges] <- getArgs
  seed <- randomIO
  putStrLn $ showGraph (randomGraph (read sNodes) (read sEdges) seed)
