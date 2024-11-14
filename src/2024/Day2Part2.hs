{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}
module Day2Part2 where

import Data.List (tails)
import Data.List.Split (splitOn)
import qualified Data.Set as S
import Prelude hiding (words)

-- needs some overlap detection...

type Coord = (Int, Int)

solve :: IO Int
solve = do
  raw <- readFile "./fixtures/day2part2.txt"
  let inputLines = lines raw
  let words = parseRunicWords (inputLines !! 0)
  let textLines = zip [1 ..] . drop 2 $ inputLines
  let res = S.size . S.unions . map (\(yCoord, line) -> countOccurrencesOfAllWords yCoord words line) $ textLines
  return res

-- input looks like "WORDS:LOR,LL,SI,OR,RU,ID,ED"
parseRunicWords :: String -> [String]
parseRunicWords = splitOn "," . drop (length "WORDS:")

countOccurrencesOfAllWords :: Int -> [String] -> String -> S.Set Coord
countOccurrencesOfAllWords yCoord words text = S.unions . map (\word -> countOccurrencesOfWord yCoord word text) $ words

countOccurrencesOfWord :: Int -> String -> String -> S.Set Coord
countOccurrencesOfWord yCoord word =
  S.fromList
    . map (\xCoord -> (xCoord, yCoord))
    . concatMap (map fst)
    . filter (\cand -> (equalForward . map snd $ cand) || (equalBackwards . map snd $ cand))
    . sliding (length word)
    . zip [1 ..]
  where
    equalForward = (== word)
    equalBackwards = (== word) . reverse

sliding :: Int -> [a] -> [[a]]
sliding n = takeWhile ((n ==) . length) . map (take n) . tails