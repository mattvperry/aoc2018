{-# LANGUAGE RecordWildCards #-}

module Day03 where

import Control.Lens
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Data.List.Split

data Claim = Claim
    { id :: Int 
    , x :: Int
    , y :: Int
    , width :: Int
    , height :: Int
    }
    deriving (Show)

parse :: String -> Claim
parse = readClaim . map read . split (dropDelims . dropBlanks $ oneOf "# @,:x")
    where readClaim [id, x, y, width, height] = Claim id x y width height

squares :: Claim -> [(Int, Int)]
squares Claim{..} = 
    [ (x + dx, y + dy)
    | dx <- [0..width - 1]
    , dy <- [0..height - 1]
    ]

overlap :: [Claim] -> Set (Int, Int)
overlap cs = M.keysSet . M.filter (>= 2) $ freq
    where freq = M.fromListWith (+) [(c, 1) | c <- concatMap squares cs]

hasOverlap :: Set (Int, Int) -> Claim -> Bool
hasOverlap o = all (`S.notMember` o) . squares

part1 :: Set (Int, Int) -> Int
part1 = length

part2 :: Set (Int, Int) -> [Claim] -> Claim
part2 o = head . filter (hasOverlap o)

main :: IO ()
main = do
    claims <- map parse . lines <$> readFile "input/Day03.txt"
    let o = overlap claims
    print $ part1 o
    print $ part2 o claims