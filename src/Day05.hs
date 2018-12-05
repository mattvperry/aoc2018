module Day05 where

import Data.Char (ord)

react :: Char -> Char -> Bool
react c c' = (abs $ ord c - ord c') == 32

sim :: String -> String
sim = foldr step []
    where step x (y:ys) | react x y = ys
          step x ys                 = x:ys

part1 :: String -> Int
part1 = length . sim

part2 :: String -> Int
part2 p = minimum . map (part1 . remove) $ zip ['a'..'z'] ['A'..'Z']
    where remove (l, u) = filter (\c -> c /= l && c /= u) p

main :: IO ()
main = do
    input <- readFile "input/Day05.txt"
    print . part1 $ input
    print . part2 $ input