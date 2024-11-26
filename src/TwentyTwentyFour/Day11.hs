module TwentyTwentyFour.Day11 (solvePart1, solvePart2) where

import Data.Function (on)
import Data.List (sortOn)
import Data.List.Extra (groupBy)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

type TermiteCollection = M.Map String Int

solvePart1 :: IO Int
solvePart1 = do
  inp <- getInput "./fixtures/day11part1.txt"
  let mappingFn = applyMapping . parseMapping $ inp
  return
    . (!! 4)
    . map countAllTermites
    . iterate mappingFn
    . M.fromList
    $ [("A", 1)]

solvePart2 :: IO Int
solvePart2 = do
  inp <- getInput "./fixtures/day11part2.txt"
  let mappingFn = applyMapping . parseMapping $ inp
  return
    . (!! 10)
    . map countAllTermites
    . iterate mappingFn
    . M.fromList
    $ [("Z", 1)]

solvePart3 = do
  inp <- getInput "./fixtures/day11part3.txt"
  let mappingFn = applyMapping . parseMapping $ inp
  return ()

countAllTermites :: TermiteCollection -> Int
countAllTermites = sum . M.elems

parseMapping :: [String] -> [(String, [String])]
parseMapping = map parseMappingLine
  where
    parseMappingLine :: String -> (String, [String])
    parseMappingLine s =
      let [a, b] = splitOn ":" s
       in (a, splitOn "," b)

applyMapping :: [(String, [String])] -> TermiteCollection -> TermiteCollection
applyMapping mapping tc = foldl1 (M.unionWith (+)) . map applyMappingLine $ mapping
  where
    applyMappingLine :: (String, [String]) -> TermiteCollection
    applyMappingLine (src, targets) = groupMapReduce id (const termitesToAdd) (+) targets
      where
        termitesToAdd = fromMaybe 0 . M.lookup src $ tc

getInput :: String -> IO [String]
getInput s = lines <$> readFile s

-- this works like scala's groupMapReduce
groupMapReduce :: (Ord k) => (a -> k) -> (a -> v) -> (v -> v -> v) -> [a] -> M.Map k v
groupMapReduce keyBy mapBy reduceBy =
  M.fromList
    . map (\xs -> (keyBy . head $ xs, foldl1 reduceBy . map mapBy $ xs))
    . groupBy ((==) `on` keyBy)
    . sortOn keyBy
