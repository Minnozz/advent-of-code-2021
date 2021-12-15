{-# LANGUAGE LambdaCase #-}
module Main where

import Data.Void (Void)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = P.Parsec Void String

data Command
  = Forward Integer
  | Down    Integer
  | Up      Integer
  deriving (Show, Eq)

parseCommands :: Parser [Command]
parseCommands = P.some command <* P.eof
  where
    command = P.choice
      [ Forward <$> (string "forward" >> integer)
      , Down    <$> (string "down"    >> integer)
      , Up      <$> (string "up"      >> integer)
      ]
    string = L.symbol P.space1
    integer = L.lexeme P.space1 L.decimal

getPos :: [Command] -> Integer
getPos commands = uncurry (*) $ foldl move start commands
  where
    start = (0, 0)
    move (pos, depth) = \case
      Forward x -> (pos + x, depth    )
      Down    x -> (pos    , depth + x)
      Up      x -> (pos    , depth - x)

getPosWithAim :: [Command] -> Integer
getPosWithAim commands =
    let (pos, depth, _aim) = foldl move start commands in
    pos * depth
  where
    start = (0, 0, 0)
    move (pos, depth, aim) = \case
      Forward x -> (pos + x, depth + aim * x, aim    )
      Down    x -> (pos    , depth          , aim + x)
      Up      x -> (pos    , depth          , aim - x)

main :: IO ()
main = do
  input <- readFile infile
  let eCommands = P.runParser parseCommands infile input
  case eCommands of
    Left errors -> print errors
    Right commands -> do
      print ("Part 1", getPos        commands)
      print ("Part 2", getPosWithAim commands)
  pure ()
  where
    infile = "res/day2/input.txt"