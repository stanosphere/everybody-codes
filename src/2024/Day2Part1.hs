module Day2Part1 where

import Data.List (tails)
import Data.List.Split (splitOn)
import Prelude hiding (words)

-- could use parsec I guess but for now I think I'll just use `sliding`, inefficient though it may be

solve :: IO Int
solve = do
  raw <- readFile "./fixtures/day2part1.txt"
  let inputLines = lines raw
  let words = parseRunicWords (inputLines !! 0)
  let text = inputLines !! 2
  return (countOccurrencesOfAllWords words text)

-- input looks like "WORDS:LOR,LL,SI,OR,RU,ID,ED"
parseRunicWords :: String -> [String]
parseRunicWords = splitOn "," . drop (length "WORDS:")

countOccurrencesOfAllWords :: [String] -> String -> Int
countOccurrencesOfAllWords words text = sum . map (`countOccurrencesOfWord` text) $ words

countOccurrencesOfWord :: String -> String -> Int
countOccurrencesOfWord word = length . filter (== word) . sliding (length word)

sliding :: Int -> [a] -> [[a]]
sliding n = takeWhile ((n ==) . length) . map (take n) . tails