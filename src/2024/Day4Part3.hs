module Day4Part3 (solve) where

import Data.List (sortOn)

solve :: IO [(Integer, Integer)]
solve = solve' <$> getInput

-- I figured the ideal height would be the average height of the nails
-- but it isn't!!!
-- I then figured it's probably near the average height at least so I just expanded my search a little either side of the average
-- I suppose this is kinda obvious if we consider a REALLY long nail, say 1000 units long, and then 10 short nails, say 1 unit long
-- The average height would be 91
-- which would mean
-- hit the ultra long nail 909 times and each of the short nails 90 times for a total of 1809 strikes
-- the obvious solution is to just hit the long nail 999 times though!!!
-- so my assumption of taking an average and assuming that's the best height is flawed
-- instead I suppose we should think about choosing an ideal height such that the total "distance" is minimised
-- I'm starting to think that this might actually just be the median
solve' :: [Integer] -> [(Integer, Integer)]
solve' xs = take 20 . sortOn snd . map (\pseudoAv -> (pseudoAv - averageHeight, sum . map (\x -> abs (x - pseudoAv)) $ xs)) . eitherSide $ averageHeight
  where
    averageHeight = average xs

eitherSide :: Integer -> [Integer]
eitherSide x = [x - 2000 .. x + 2000]

average :: [Integer] -> Integer
average xs = sum xs `div` (toInteger . length $ xs)

getInput :: IO [Integer]
getInput = map read . lines <$> readFile "./fixtures/day4part3.txt"
