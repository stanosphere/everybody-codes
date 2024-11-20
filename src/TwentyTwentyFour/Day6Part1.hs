module TwentyTwentyFour.Day6Part1 (getPathOfUniqueLength, solve, buildTree, parseInput, findAllPaths) where

-- parse into Tree using unfoldTree
-- find the paths in the tree
-- - could use foldTree
-- - failing that just "normal" recursion
-- find the path with the unique length
-- may need to worry about leaf nodes that are not '@'

import Data.Foldable (find)
import Data.Function (on)
import Data.List (sortOn)
import Data.List.Extra (groupBy)
import Data.List.Split (splitOn)
import qualified Data.Map as M (Map, fromList)
import Data.Tree (Tree (Node), unfoldTree)

solve :: IO (Maybe String)
solve =
  getPathOfUniqueLength
    . findAllPathStrings
    . buildTree
    . parseInput
    <$> getInput

getPathOfUniqueLength :: [String] -> Maybe String
getPathOfUniqueLength = fmap head . find ((== 1) . length) . groupBy' length

findAllPathStrings :: Tree String -> [String]
findAllPathStrings = map (concat . reverse) . findAllPaths

findAllPaths :: Tree a -> [[a]]
findAllPaths = appendPaths []
  where
    appendPaths :: [a] -> Tree a -> [[a]]
    appendPaths p (Node label []) = [label : p]
    appendPaths p (Node label children) = concatMap (appendPaths (label : p)) children

buildTree :: [(String, [String])] -> Tree String
buildTree xs = unfoldTree getChildren "RR"
  where
    getChildren :: String -> (String, [String])
    getChildren nodeLabel = (nodeLabel, maybe [] snd . find ((== nodeLabel) . fst) $ xs)

parseInput :: [String] -> [(String, [String])]
parseInput = map parseInputLine
  where
    parseInputLine :: String -> (String, [String])
    parseInputLine s =
      let [parent, children] = splitOn ":" s
       in (parent, splitOn "," children)

getInput :: IO [String]
getInput = lines <$> readFile "./fixtures/day6part1.txt"

-- this works like scala's groupBy in the sense that elements need not be adjacent in the original list to be grouped
groupBy' :: (Ord k) => (a -> k) -> [a] -> M.Map k [a]
groupBy' f =
  M.fromList
    . map (\xs -> (f . head $ xs, xs))
    . groupBy ((==) `on` f)
    . sortOn f
