module Day01 where

import Data.Set (Set)
import qualified Data.Set as S

parse :: String -> Int
parse = read . filter (/= '+')

dupe :: Set Int -> [Int] -> Int
dupe seen (x:xs)
    | x `S.member` seen = x
    | otherwise         = dupe (S.insert x seen) xs
dupe _ _ = error "duplicate not found"

part1 :: [Int] -> Int
part1 = sum

part2 :: [Int] -> Int
part2 = dupe S.empty . scanl (+) 0 . cycle

main :: IO ()
main = do
    input <- map parse . lines <$> readFile "input/Day01.txt"
    print . part1 $ input
    print . part2 $ input