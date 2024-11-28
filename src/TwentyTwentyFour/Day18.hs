module TwentyTwentyFour.Day18 (solve') where

import Data.List (find)
import qualified Data.Set as S

type Coord = (Int, Int)

type Grid = S.Set Coord

data GridState = GS
  { _unvisited :: Grid,
    _justVisited :: Grid,
    _time :: Int
  }
  deriving (Show)

solve' :: IO (Maybe Int)
solve' = do
  inp <- getInput
  let (grid, palmLocations) = parseInput inp
  let initState = GS (S.delete (0, 1) grid) (S.singleton (0, 1)) 0
  let res = solve initState palmLocations
  return res

solve :: GridState -> S.Set Coord -> Maybe Int
solve initState palmTreeLocations = fmap _time . find isFinished . iterate evolveState $ initState
  where
    isFinished g = S.intersection palmTreeLocations (_unvisited g) == S.empty

evolveState :: GridState -> GridState
evolveState gs = GS unvisited justVisited time
  where
    justVisited = getUnvisitedNeighbours gs
    unvisited = _unvisited gs S.\\ justVisited
    time = _time gs + 1

getUnvisitedNeighbours :: GridState -> S.Set Coord
getUnvisitedNeighbours (GS unvisited justVisited _) =
  S.intersection unvisited . S.unions . S.map neighbours $ justVisited
  where
    neighbours :: Coord -> S.Set Coord
    neighbours (x0, y0) =
      S.fromList
        [ (x0 + 1, y0),
          (x0 - 1, y0),
          (x0, y0 + 1),
          (x0, y0 - 1)
        ]

parseInput :: [String] -> (S.Set Coord, S.Set Coord)
parseInput xs =
  ( S.fromList . map fst $ grid,
    S.fromList . map fst . filter ((== 'P') . snd) $ grid
  )
  where
    zipWithIndex :: [a] -> [(Int, a)]
    zipWithIndex = zip [0 ..]
    grid =
      [ ((x, y), value)
        | (y, rows) <- zipWithIndex xs,
          (x, value) <- zipWithIndex rows,
          value /= '#'
      ]

getInput :: IO [String]
getInput = lines <$> readFile "./fixtures/day18part1.txt"
