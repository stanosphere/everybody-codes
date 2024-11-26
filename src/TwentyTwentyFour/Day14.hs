module TwentyTwentyFour.Day14 (solvePart1) where

import Data.List.Split (splitOn)

data Command = Cmd
  { _dir :: Char,
    _steps :: Int
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

parseInputLine :: String -> [Command]
parseInputLine = map (\c -> Cmd (head c) (read . tail $ c)) . splitOn ","
