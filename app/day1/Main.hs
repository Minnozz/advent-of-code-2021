module Main where

import Data.List ( transpose )

slidingWindow :: Int -> [a] -> [[a]]
slidingWindow size list =
    transpose $ [subList i | i <- [0..size-1]]
  where
    subList i = (take len . drop i) list
    len = length list - size + 1

part1 :: [Int] -> Int
part1 depths =
    length $ filter id increases
  where
    increases = zipWith (>) (tail depths) depths

part2 :: [Int] -> Int
part2 depths =
    length $ filter (uncurry (>)) pairs
  where
    sums = sum <$> slidingWindow 3 depths
    pairs = zip (tail sums) sums

main :: IO ()
main = do
    bytes <- readFile "res/day1/input.txt"
    let depths = read <$> lines bytes
    print $ part1 depths
    print $ part2 depths
    pure ()