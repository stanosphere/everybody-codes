module TwentyTwentyFour.Day6Part2 (solve) where

import TwentyTwentyFour.Day6Part1 (buildTree, findAllPaths, getPathOfUniqueLength, parseInput)

-- hmm I think given the relative lack of abundance of @'s I should have just done bottom up kinda vibes,ahh well!
solve :: IO (Maybe String)
solve = getPathOfUniqueLength . simplifyPaths . findAllPaths . buildTree . parseInput <$> getInput

simplifyPaths :: [[String]] -> [String]
simplifyPaths = filter ((== '@') . last) . map simplifyPath
  where
    simplifyPath :: [String] -> String
    simplifyPath = map head . reverse

getInput :: IO [String]
getInput = lines <$> readFile "./fixtures/day6part2.txt"
