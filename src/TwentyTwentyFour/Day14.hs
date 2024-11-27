module TwentyTwentyFour.Day14 (solvePart1, solvePart2) where

import Data.List.Split (splitOn)
import qualified Data.Set as S

type Coord = (Int, Int, Int)

data Command = Cmd
  { _dir :: Char,
    _steps :: Int
  }

data PlantState = PS
  { _position :: Coord,
    _occupied :: S.Set Coord
  }

solvePart1 :: IO Int
solvePart1 = do
  inp <- readFile "./fixtures/day14part1.txt"
  return . getMaxHeight . parseInputLine $ inp
  where
    getMaxHeight :: [Command] -> Int
    getMaxHeight = maximum . scanl newHeight 0
      where
        newHeight :: Int -> Command -> Int
        newHeight h (Cmd 'U' x) = h + x
        newHeight h (Cmd 'D' x) = h - x
        newHeight h _ = h

solvePart2 = do
  inp <- fmap lines . readFile $ "./fixtures/day14part2.txt"
  let res = S.size . S.unions . map (getAllCoords . parseInputLine) $ inp
  return res

getAllCoords :: [Command] -> S.Set Coord
getAllCoords = _occupied . foldl folder (PS (0, 0, 0) S.empty)
  where
    folder :: PlantState -> Command -> PlantState
    folder (PS pos oc) cmd =
      let newCoords = commandToCoords pos cmd
       in PS (last newCoords) (S.union oc . S.fromList $ newCoords)

commandToCoords :: Coord -> Command -> [Coord]
commandToCoords (x, y, z) (Cmd 'U' s) = map (\a -> (x, y, z + a)) [1 .. s]
commandToCoords (x, y, z) (Cmd 'D' s) = map (\a -> (x, y, z - a)) [1 .. s]
commandToCoords (x, y, z) (Cmd 'R' s) = map (\a -> (x, y + a, z)) [1 .. s]
commandToCoords (x, y, z) (Cmd 'L' s) = map (\a -> (x, y - a, z)) [1 .. s]
commandToCoords (x, y, z) (Cmd 'F' s) = map (\a -> (x + a, y, z)) [1 .. s]
commandToCoords (x, y, z) (Cmd 'B' s) = map (\a -> (x - a, y, z)) [1 .. s]
commandToCoords _ _ = undefined

parseInputLine :: String -> [Command]
parseInputLine = map (\c -> Cmd (head c) (read . tail $ c)) . splitOn ","
