module TwentyTwentyFour.Day1Part3 (solve) where

import Data.List.Split (chunksOf)

solve :: IO Int
solve = fmap getTotalPotions <$> readFile $ "./fixtures/day1part3.txt"

getTotalPotions :: [Char] -> Int
getTotalPotions = sum . map tripleToPotionsRequired . chunksOf 3

toPotionsRequired :: Char -> Int
toPotionsRequired 'A' = 0
toPotionsRequired 'B' = 1
toPotionsRequired 'C' = 3
toPotionsRequired 'D' = 5
toPotionsRequired 'x' = 0
toPotionsRequired unexpected = error ("unexpected letter: " ++ [unexpected])

tripleToPotionsRequired :: [Char] -> Int
tripleToPotionsRequired xs = scoreFromCreatures + totalCreatures * (totalCreatures - 1)
  where
    totalCreatures = length . filter (/= 'x') $ xs
    scoreFromCreatures = sum . map toPotionsRequired . filter (/= 'x') $ xs
