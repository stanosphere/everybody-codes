{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}
module Day2Part3 where

import Data.List (tails)
import Data.List.Extra (transpose)
import Data.List.Split (splitOn)
import qualified Data.Set as S
import Prelude hiding (words)

-- kinda like part 2 but I'll just do some transposes and stuff...
-- oh and modular stuff too because of the wrap around...
-- OR I could do what I did in that wordsearch problem a while back. IDK

type Coord = (Int, Int)

solve = do
  raw <- readFile "./fixtures/day2part3.txt"
  let inputLines = lines raw
  let words = parseRunicWords (inputLines !! 0)
  let textLines = drop 2 $ inputLines
  let res1 = S.size . S.unions . map (\(yCoord, line) -> countOccurrencesOfAllWords yCoord words line) . zip [1 ..] $ textLines
  let res2 = S.size . S.unions . map (\(yCoord, line) -> countOccurrencesOfAllWords yCoord words line) . zip [1 ..] . transpose $ textLines
  print res1
  print res2

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