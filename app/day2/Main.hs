{-# LANGUAGE LambdaCase #-}
module Main where

import Data.Void (Void)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = P.Parsec Void String

data Command
  = Forward Integer
  | Down Integer
  | Up Integer
  deriving (Show, Eq)

parseCommands :: Parser [Command]
parseCommands = P.some command <* P.eof
  where
    command = P.choice
      [ Forward <$> (P.string "forward " >> L.decimal),
        Down <$> (P.string "down " >> L.decimal),
        Up <$> (P.string "up " >> L.decimal)
      ]
      <* P.newline

getPos :: [Command] -> Integer
getPos commands = uncurry (*) $ foldl move start commands
  where
    start = (0, 0)
    move (pos, depth) = \case
      Forward x -> (pos + x, depth)
      Down x -> (pos, depth + x)
      Up x -> (pos, depth - x)

main :: IO ()
main = do
  input <- readFile infile
  let eCommands = P.runParser parseCommands infile input
  case eCommands of
    Left errors -> print errors
    Right commands -> do
      print commands
      print ("Part 1", getPos commands)
  pure ()
  where
    infile = "res/day2/input.txt"