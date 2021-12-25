module Main where
import Data.List (transpose)

fromBinaryString :: String -> Integer
fromBinaryString str = sum $ zipWith val [0..] $ reverse str
  where
    val :: Integer -> Char -> Integer
    val exp '0' = 0
    val exp '1' = 2 ^ exp
    val exp _ = error "invalid binary character"

invertBinaryString :: String -> String
invertBinaryString [] = []
invertBinaryString ('1':xs) = '0':invertBinaryString xs
invertBinaryString ('0':xs) = '1':invertBinaryString xs
invertBinaryString _ = error "invalid binary character"

getPowerConsumption :: [String] -> Integer
getPowerConsumption lines = gamma * epsilon
  where
    gamma   = fromBinaryString mostCommon
    epsilon = fromBinaryString $ invertBinaryString mostCommon

    mostCommon = map mostCommonChar $ transpose lines

    mostCommonChar :: String -> Char
    mostCommonChar chars =
        if length ones * 2 > length chars
        then '1'
        else '0'
      where
        ones = filter (=='1') chars

main :: IO ()
main = do
  lines <- lines <$> readFile infile
  print ("Part 1", getPowerConsumption lines)
  pure ()
  where
    infile = "res/day3/input.txt"