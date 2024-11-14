module Day1Part1 (solve) where

solve :: IO Int
solve = fmap getTotalPotions <$> readFile $ "./fixtures/day1part1.txt"

getTotalPotions :: [Char] -> Int
getTotalPotions = sum . map toPotionsRequired

toPotionsRequired :: Char -> Int
toPotionsRequired 'A' = 0
toPotionsRequired 'B' = 1
toPotionsRequired 'C' = 3
toPotionsRequired _ = undefined
