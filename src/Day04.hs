module Day04 where

import Data.Ord
import Data.List
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as M

data Time = Time
    { month :: Int
    , day :: Int
    , hour :: Int
    , minute :: Int
    } deriving (Show, Eq, Ord)

data Event = Start Int | Sleep | Wake deriving (Show)

isStart :: Event -> Bool
isStart (Start _) = True
isStart _         = False

readInput :: String -> IO [(Time, Event)]
readInput f = toEvents . lines <$> readFile f
    where toEvents = sortBy (comparing fst) . map parse

parse :: String -> (Time, Event)
parse s = (Time (read m) (read d) (read h) (read mm), toEvent rest)
    where (_:m:d:h:mm:rest) = split (dropDelims . dropBlanks $ oneOf " #[]-:") s
          toEvent ["Guard", id, _, _]   = Start (read id)
          toEvent ["falls", _]          = Sleep
          toEvent ["wakes", _]          = Wake

analyzeShift :: [(Time, Event)] -> (Int, [Int])
analyzeShift z@((_, Start id):es) = (id, minutes es)
    where minutes [] = []
          minutes ((ts, Sleep):(ta, Wake):xs) = [minute ts..(minute ta) - 1] ++ minutes xs

aggregate :: [(Time, Event)] -> Map Int (Map Int Int)
aggregate = M.map freq . M.fromListWith (++) . map analyzeShift . toShifts
    where freq xs = M.fromListWith (+) [(x, 1) | x <- xs]
          toShifts = split (keepDelimsL . dropInitBlank $ whenElt (isStart . snd))

solve :: ([Int] -> Int) -> Map Int (Map Int Int) -> Int
solve f gs = (fst . maximumBy (comparing snd) . M.toList . snd $ target) * (fst target)
    where target = maximumBy (comparing maxMinutes) . filter (not . null . snd) . M.toList $ gs
          maxMinutes = f . M.elems . snd

part1 :: Map Int (Map Int Int) -> Int
part1 = solve sum

part2 :: Map Int (Map Int Int) -> Int
part2 = solve maximum

main :: IO ()
main = do
    input <- aggregate <$> readInput "input/Day04.txt"
    print . part1 $ input
    print . part2 $ input