module TwentyTwentyFour.Day3Part1 (solve) where

import Data.List (find)
import qualified Data.Map as M (Map, elems, filter, fromList, insert, keys, lookup, null)
import Data.Maybe (fromMaybe)

type Coord = (Int, Int)

type Grid = M.Map Coord Int

data GridState = GridState
  { grid :: Grid,
    depth :: Int
  }
  deriving (Show)

solve :: IO (Maybe Int)
solve = do
  rawInput <- getInput
  return
    . fmap (sum . M.elems . grid)
    . find shouldStop
    . iterate evolveGridState
    $ GridState (getInitialGrid rawInput) 1

evolveGridState :: GridState -> GridState
evolveGridState (GridState g d) = GridState (evolveGrid d g) (d + 1)
  where
    evolveGrid :: Int -> Grid -> Grid
    evolveGrid prevMaxDepth g =
      foldr (\coord -> M.insert coord (prevMaxDepth + 1)) g
        . filter (\c -> all (== prevMaxDepth) . getNeighbourHeights c $ g)
        . M.keys
        . M.filter (== prevMaxDepth)
        $ g

getNeighbourHeights :: Coord -> Grid -> [Int]
getNeighbourHeights (x0, y0) g =
  map
    (fromMaybe 0)
    [ M.lookup (x0 + 1, y0) g,
      M.lookup (x0 - 1, y0) g,
      M.lookup (x0, y0 + 1) g,
      M.lookup (x0, y0 - 1) g,
      -- diags below here, comment them out to get part 1 to work
      M.lookup (x0 + 1, y0 + 1) g,
      M.lookup (x0 - 1, y0 - 1) g,
      M.lookup (x0 + 1, y0 - 1) g,
      M.lookup (x0 - 1, y0 + 1) g
    ]

shouldStop :: GridState -> Bool
shouldStop (GridState g prevMaxDepth) = M.null . M.filter (== prevMaxDepth) $ g

getInitialGrid :: [String] -> Grid
getInitialGrid xs =
  M.fromList
    [ ((x, y), 1)
      | (y, rows) <- zipWithIndex xs,
        (x, value) <- zipWithIndex rows,
        value == '#'
    ]
  where
    zipWithIndex :: [a] -> [(Int, a)]
    zipWithIndex = zip [0 ..]

getInput = lines <$> readFile "./fixtures/day3part1.txt"
