{-# LANGUAGE RankNTypes #-}

module TwentyTwentyFour.Day17 (solvePart1, solvePart2) where

import Data.List (sort, sortOn)
import Data.List.Extra (nubOrdOn)
import qualified Data.Map as M
import qualified Data.Set as S

-- so I think this is actually just "find the minimum spanning tree"
-- I used to know how to do this when I did A level maths lol
-- but now I don't so will need to look it up!
-- Either way I think a sensible first step will be to just parse the input into a graph

-- the big ones are Prim and Kruskal
-- in fact I've quite possibly had to implement one of them before in Haskell for advent of code...

type Coord = (Int, Int)

type Edge' = (Coord, Coord, Int)

solvePart1 :: IO Int
solvePart1 = do
  x <- getInput "./fixtures/day17part1.txt"
  return . getConstellationSize . kruskal . mkEdges . getStarPositions $ x

solvePart2 :: IO Int
solvePart2 = do
  x <- getInput "./fixtures/day17part2.txt"
  return . getConstellationSize . kruskal . mkEdges . getStarPositions $ x

getConstellationSize :: [Edge'] -> Int
getConstellationSize xs = totalStars + totalEdges
  where
    totalStars = 1 + length xs
    totalEdges = sum . map (\(_, _, v) -> v) $ xs

kruskal :: [Edge'] -> [Edge']
kruskal xs = snd . foldl folder (initUf, []) $ sortedEdges
  where
    sortedEdges = sortOn (\(_, _, v) -> v) xs
    initUf = fromList . concatMap (\(x, y, _) -> [x, y]) $ xs
    folder :: (UnionFind Coord, [Edge']) -> Edge' -> (UnionFind Coord, [Edge'])
    folder (uf, mst) (x, y, v) =
      if inSameCluster uf x y
        then (uf, mst)
        else (unionClusters uf x y, (x, y, v) : mst)

-- using Manhattan metric
-- this'll just be a carty prod with deduplications
mkEdges :: [Coord] -> [Edge']
mkEdges xs =
  nubOrdOn discrim [(from, to, calcValue from to) | from <- xs, to <- xs]
  where
    calcValue (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)
    discrim (x, y, _) = sort [x, y]

getStarPositions :: [String] -> [Coord]
getStarPositions xs =
  [ (x, y)
    | (y, rows) <- zipWithIndex xs,
      (x, value) <- zipWithIndex rows,
      value == '*'
  ]
  where
    zipWithIndex :: [a] -> [(Int, a)]
    zipWithIndex = zip [0 ..]

getInput s = lines <$> readFile s

-- I mean maybe this below here should live in its own file...

-- find the leader of a cluster of elements
-- union clusters of elements
data UnionFind a = UF
  { _leaderMap :: M.Map a a,
    _clusterMap :: M.Map a (S.Set a)
  }

fromList :: (Ord a) => [a] -> UnionFind a
fromList xs = UF leaderMap clusterMap
  where
    leaderMap = M.fromList (zip xs xs)
    clusterMap = M.fromList (zip xs (map S.singleton xs))

inSameCluster :: (Ord a) => UnionFind a -> a -> a -> Bool
inSameCluster uf x y = clusterLeader uf x == clusterLeader uf y

clusterLeader :: (Ord a) => UnionFind a -> a -> a
clusterLeader uf x = _leaderMap uf M.! x

unionClusters :: (Ord a) => UnionFind a -> a -> a -> UnionFind a
unionClusters uf x y =
  -- should really only move stuff over in the smaller cluster tbh...
  -- but for this problem probably don't need to worry too much
  if xLeader == yLeader then uf else UF newLeaderMap newClusterMap
  where
    xLeader = clusterLeader uf x
    yLeader = clusterLeader uf y
    -- only need stuff below here if x and y are in different clusters
    clusterX = _clusterMap uf M.! xLeader
    clusterY = _clusterMap uf M.! yLeader
    unionCluster = S.union clusterX clusterY
    newClusterMap = M.delete yLeader . M.insert xLeader unionCluster . _clusterMap $ uf
    newLeaderMap = foldr (`M.insert` xLeader) (_leaderMap uf) . S.toList $ clusterY

-- https://stackoverflow.com/questions/4290163/how-can-i-write-a-mst-algorithm-prim-or-kruskal-in-haskell
-- this one I don't really get so I won't use it other than for checking my own implementation
-- turns out I just got my implementation right first time lol
-- kruskal' :: (Ord a) => ((Vertex, Vertex) -> a) -> Graph -> [(Vertex, Vertex)]
-- kruskal' weightFn graph = run $ filterM go (sortBy (comparing weightFn) theEdges)
--   where
--     theEdges = G.edges graph
--     go (u, v) = do
--       eq <- equivalent u v
--       if eq then return False else equate u v >> return True
--     run = runEquivM (const ()) (const $ const ())