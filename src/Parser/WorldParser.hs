{-|
Module: Parser.WorldParser
Description: Defines the parser to a save file into a WorldWithoutGUI.

The 'worldParser' function is used to parse a save file into a WorldWithoutGUI.
-}
module Parser.WorldParser(
  -- * Parser
  worldParser
) where

import Text.Parsec
import Types
import Network (HostName, Socket)
import GHC.IO.Handle (Handle)
import Debug.Trace

-- | Parse a save file into a worldWithoutGUI
worldParser :: Parsec String () WorldWithoutGUI
worldParser = do
  b <- boardParser
  gm <- modeParser
  c <- colParser
  string "["
  pbs <- many boardParser
  string "]"
  WorldWithoutGUI InPlay b gm c Nothing "" "" pbs <$> clockParser


-- parsec parser for board - Show board creates "Board {size = int, target = int, pieces = [(pos, col)]}"
boardParser :: Parsec String () Board
boardParser = do
  string "Board {size = "
  sz <- intParser
  string ", target = "
  tgt <- intParser
  string ", pieces = ["
  pcs <- many pieceParser
  string "]}"
  string "," <|> string ""
  return (Board sz tgt pcs)

-- parsec parser for piece, piece can have comma or not at the end
pieceParser :: Parsec String () (Position, Col)
pieceParser = do
  string "(("
  x <- intParser
  string ","
  y <- intParser
  string "),"
  col <- colParser
  string ")"
  string "," <|> string ""
  return ((x, y), col)

-- parsec parser for position
posParser :: Parsec String () Position
posParser = do
  string "((("
  x <- intParser
  string ","
  y <- intParser
  string "),"
  return (x, y)

clockParser :: Parsec String () (Maybe (Int, Int, Int))
clockParser = do
    try (do string "Just ("
            cl1 <- intParser
            string ","
            cl2 <- intParser
            string ","
            cl3 <- intParser
            string ")"
            return (Just (cl1, cl2, cl3)))
    <|> return Nothing

-- parsec parser for colour
colParser :: Parsec String () Col
colParser = do
  col <- string "White" <|> string "Black"
  return (read col)

-- parsec parser for game mode
modeParser :: Parsec String () GameMode
modeParser = do
    mode <- string "AI" <|> string "Local" <|> string "Online"
    return (read mode)

-- parsec parser for bool
boolParser :: Parsec String () Bool
boolParser = do
  bool <- string "True" <|> string "False"
  return (read bool)

-- parsec parser for int
intParser :: Parsec String () Int
intParser = do
  int <- many1 digit
  return (read int)
