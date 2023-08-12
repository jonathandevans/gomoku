{-|
Module: Parser.ArgumentParser
Description: Defines the parser to read program arguments into world settings.

The 'argumentParser' function is used to turn arguments into a tuple of world parameters.
-}
module Parser.ArgumentParser(
  -- * Parser
  argumentParser
) where

import Text.Parsec
import Network.Socket

import Types

-- | Parsec parser for the arguments passed to the program
argumentParser :: (GameState, Int, Int, GameMode, Col, HostName, Maybe(Int, Int, Int)) -> Parsec String () (GameState, Int, Int, GameMode, Col, HostName, Maybe(Int, Int, Int))
argumentParser (a,a1,a2,a3,a4,a5,a6) =
    try (do s <- sizeParser
            argumentParser (InPlay, s, a2, a3, a4, a5, a6))
    <|> try (do t <- targetParser
                argumentParser (InPlay, a1, t, a3, a4, a5, a6))
    <|> try (do gm <- gameModeParser
                argumentParser (InPlay, a1, a2, gm, a4, a5, a6))
    <|> try (do c <- colourParser
                argumentParser (InPlay, a1, a2, a3, c, a5, a6))
    <|> try (do h <- hostParser
                argumentParser (InPlay, a1, a2, a3, a4, h, a6))
    <|> return (a,a1,a2,a3,a4,a5,a6)

-- parsec parser for size argument
sizeParser :: Parsec String () Int
sizeParser = do
    string "size"
    string "="
    i <- intParser
    spaces
    return i

-- parsec parser for target argument
targetParser :: Parsec String () Int
targetParser = do
    string "target"
    string "="
    i <- intParser
    spaces
    return i

-- parsec parser for mode argument
gameModeParser :: Parsec String () GameMode
gameModeParser = do
    string "mode"
    string "="
    m <- modeParser
    spaces
    return m

-- parsec parser for host argument
hostParser :: Parsec String () HostName
hostParser = do
    string "host"
    string "="
    h <- many1 (noneOf " ")
    spaces
    return h

-- parsec parser for colour argument
colourParser :: Parsec String () Col
colourParser = do
    string "colour"
    string "="
    c <- colParser
    spaces
    return c

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

-- parsec parser for int
intParser :: Parsec String () Int
intParser = do
  int <- many1 digit
  return (read int)