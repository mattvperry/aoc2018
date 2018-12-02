module Day02 where

import Data.Ord (comparing)
import Data.List (minimumBy)
import qualified Data.Map as M

freq :: String -> [Int]
freq s = M.elems $ M.fromListWith (+) [(c, 1) | c <- s]

checksum :: (Int, Int) -> [[Int]] -> Int
checksum (a, b) [] = a * b
checksum (a, b) (x:xs)
    | 2 `elem` x && 3 `elem` x = checksum (a + 1, b + 1) xs
    | 2 `elem` x = checksum (a + 1, b) xs
    | 3 `elem` x = checksum (a, b + 1) xs
    | otherwise  = checksum (a, b) xs

pairs :: [String] -> [(String, String)]
pairs xs = [(x, x') | x <- xs, x' <- xs, x /= x']

dist :: (String, String) -> Int
dist = length . filter not . uncurry (zipWith (==))

part1 :: [String] -> Int
part1 = checksum (0, 0) . map freq

part2 :: [String] -> String
part2 = map fst . filter (uncurry (==)) . uncurry zip . minimumBy (comparing dist) . pairs

main :: IO ()
main = do
    input <- lines <$> readFile "input/Day02.txt"
    print . part1 $ input
    print . part2 $ input