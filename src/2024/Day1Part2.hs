module Day1Part2 (solve) where

import Data.List.Split (chunksOf)

solve :: IO Int
solve = fmap getTotalPotions <$> readFile $ "./fixtures/day1part2.txt"

getTotalPotions :: [Char] -> Int
getTotalPotions = sum . map pairToPotionsRequired . pairs

toPotionsRequired :: Char -> Int
toPotionsRequired 'A' = 0
toPotionsRequired 'B' = 1
toPotionsRequired 'C' = 3
toPotionsRequired 'D' = 5
toPotionsRequired 'x' = 0
toPotionsRequired unexpected = error ("unexpected letter: " ++ [unexpected])

pairToPotionsRequired :: (Char, Char) -> Int
pairToPotionsRequired ('x', p) = toPotionsRequired p
pairToPotionsRequired (p, 'x') = toPotionsRequired p
pairToPotionsRequired (p1, p2) = 2 + toPotionsRequired p1 + toPotionsRequired p2

pairs :: [a] -> [(a, a)]
pairs = map toTuple . chunksOf 2
  where
    toTuple :: [a] -> (a, a)
    toTuple [x1, x2] = (x1, x2)
    toTuple _ = error "wrong number of elements in sub-list"