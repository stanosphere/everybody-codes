{-# HLINT ignore "Use tuple-section" #-}
module Day2Part3 where

import Data.List (tails)
import Data.List.Extra (transpose)
import Data.List.Split (splitOn)
import qualified Data.Set as S
import Data.Tuple (swap)
import Prelude hiding (words)

-- kinda like part 2 but I'll just do some transposes and stuff...
-- oh and modular stuff too because of the wrap around...
-- OR I could do what I did in that wordsearch problem a while back. IDK

type Coord = (Int, Int)

solve :: IO Int
solve = do
  raw <- readFile "./fixtures/day2part3.txt"
  let inputLines = lines raw
  let words = parseRunicWords . head $ inputLines
  let textLines = drop 2 inputLines
  let res1 =
        S.unions
          . zipWith (`countOccurrencesOfAllWordsHoriz` words) [1 ..]
          $ textLines
  let res2 =
        S.map swap
          . S.unions
          . zipWith (`countOccurrencesOfAllWordsVert` words) [1 ..]
          . transpose
          $ textLines
  return . S.size $ S.union res1 res2

-- input looks like "WORDS:LOR,LL,SI,OR,RU,ID,ED"
parseRunicWords :: String -> [String]
parseRunicWords = splitOn "," . drop (length "WORDS:")

-- dashed version does horiz stuff
countOccurrencesOfAllWordsHoriz :: Int -> [String] -> String -> S.Set Coord
countOccurrencesOfAllWordsHoriz yCoord words text = S.unions . map (\word -> countOccurrencesOfWordHoriz yCoord word text) $ words

-- "normal" version does vertical stuff
countOccurrencesOfAllWordsVert :: Int -> [String] -> String -> S.Set Coord
countOccurrencesOfAllWordsVert yCoord words text = S.unions . map (\word -> countOccurrencesOfWordVert yCoord word text) $ words

countOccurrencesOfWordVert :: Int -> String -> String -> S.Set Coord
countOccurrencesOfWordVert yCoord word =
  S.fromList
    . map (\xCoord -> (xCoord, yCoord))
    . concatMap (map fst)
    . filter (\cand -> (equalForward . map snd $ cand) || (equalBackwards . map snd $ cand))
    . sliding (length word)
    . zip [1 ..]
  where
    equalForward = (== word)
    equalBackwards = (== word) . reverse

countOccurrencesOfWordHoriz :: Int -> String -> String -> S.Set Coord
countOccurrencesOfWordHoriz yCoord word text =
  S.fromList
    . map (\xCoord -> (xCoord, yCoord))
    . concatMap (map fst)
    . filter (\cand -> (equalForward . map snd $ cand) || (equalBackwards . map snd $ cand))
    $ doubleText
  where
    equalForward = (== word)
    equalBackwards = (== word) . reverse
    -- just doubling the text for ease, but I could get away with using length of word couldn't I...
    doubleText = sliding (length word) . concat . replicate 2 . zip [1 ..] $ text

sliding :: Int -> [a] -> [[a]]
sliding n = takeWhile ((n ==) . length) . map (take n) . tails