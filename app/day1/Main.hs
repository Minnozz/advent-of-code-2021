module Main where

import Data.List ( transpose )

slidingWindow :: Int -> [a] -> [[a]]
slidingWindow size list =
    take len $ transpose $ map subList [0..size-1]
  where
    subList i =  drop i list
    len = length list - size + 1

countIncreases :: Int -> [Int] -> Int
countIncreases windowSize depths =
    length $ filter (uncurry (>)) pairs
  where
    sums = sum <$> slidingWindow windowSize depths
    pairs = zip (tail sums) sums

main :: IO ()
main = do
    bytes <- readFile "res/day1/input.txt"
    let depths = read <$> lines bytes
    print ("Part 1", countIncreases 1 depths)
    print ("Part 2", countIncreases 3 depths)
    pure ()