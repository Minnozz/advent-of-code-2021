module Main where

import Data.List

day1part1 :: [Int] -> Int
day1part1 depths =
    length $ filter id increases
  where
    increases = zipWith (>) (tail depths) depths

day1part2 :: [Int] -> Int
day1part2 depths =
    length $ filter id increases
  where
    windows = filter ((== 3) . length) $ transpose
      [ depths
      , tail depths
      , tail $ tail depths
      ]
    sums = sum <$> windows
    increases = zipWith (>) (tail sums) sums

main :: IO ()
main = do
    day1 <- readFile "res/day1/input.txt"
    let day1depths = read <$> lines day1
    print $ day1part1 day1depths
    print $ day1part2 day1depths
    pure ()