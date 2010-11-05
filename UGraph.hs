module UGraph (Node, nodes, edges, -- re-exports from Data.Graph.Inductive
  UGraph, cv, randomGraph, saveGraph, loadGraph, unique)
where

import Control.Monad
import Data.Graph.Inductive
import System.Random

import Util

type UGraph = Gr () ()

cv :: UGraph -> (Node, Node) -> Int
cv g (i, j) = boolToInt $ j `elem` neighbors g i

randomGraph :: Int -> Int -> Int -> UGraph
randomGraph nodes edges seed = mkUGraph [1..nodes] $ take edges rEdges
  where rEdges = filter (\(i,j) -> i /= j) $ zip rNodes (tail rNodes)
        rNodes = randomRs (1, nodes) (mkStdGen seed)

saveGraph :: UGraph -> FilePath -> IO ()
saveGraph g file = writeFile file $ show (nodes g, edges g)

loadGraph :: FilePath -> IO UGraph
loadGraph file = do
  (nodes, edges) <- liftM read $ readFile file
  return $ mkUGraph nodes edges

unique :: [Edge] -> [Edge]
unique [e] = [e]
unique ((i, j):es) = if (i, j) `elem` es || (j, i) `elem` es then unique es else
  (i, j) : unique es
