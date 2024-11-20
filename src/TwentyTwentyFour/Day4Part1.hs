module TwentyTwentyFour.Day4Part1 (solve) where

-- NOTE: this also works for part 2,just change the file you're reading!

solve :: IO Int
solve = solve' <$> getInput

solve' :: [Int] -> Int
solve' xs = sum . map (\x -> x - minHeight) $ xs
  where
    minHeight = minimum xs

getInput :: IO [Int]
getInput = map read . lines <$> readFile "./fixtures/day4part2.txt"
