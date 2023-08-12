{-|
Module: Parser.SGFParser
Description: Defines the parser to read sgf files into a list of WorldWithoutGUI.

The 'sgfParser' function is used to turn an sgf file into a list of WorldWithoutGUI that can be moved through to show a game.
-}
module Parser.SGFParser(
  -- * Parser
  sgfParser,
  -- * Helper function to convert a number to a letter
  convertNumber
) where

import Text.Parsec
import Data.Char (chr, ord, toLower)

import Types

-- | Parsec parser for the sgf file
sgfParser :: Parsec String () [WorldWithoutGUI]
sgfParser = do
    b <- gameConfigParser
    w <- moveParser b []
    return w

-- parsec parser for the game config in the sgf file
gameConfigParser :: Parsec String () WorldWithoutGUI
gameConfigParser = do
    string "(;"
    string "GM[1]FF[4]CA[UTF-8]AP[Gomoku:0.1]\n"
    string "DT["
    manyTill anyChar (try (string "]\n"))
    size <- sizeParser
    t <- timeLimitParser
    manyTill anyChar (try (string ")\n"))
    case t of
        Just t' -> return $ WorldWithoutGUI (Playback [] 0) (Board size 0 []) Local Black Nothing "" "" [] (Just (0,t',t'))
        Nothing -> return $ WorldWithoutGUI (Playback [] 0) (Board size 0 []) Local Black Nothing "" "" [] Nothing

-- parsec parser for the size of the board in the sgf file
sizeParser :: Parsec String () Int
sizeParser = do
    string "SZ["
    size <- intParser
    string "]\n"
    return size

-- parsec parser for the time limit in the sgf file
timeLimitParser :: Parsec String () (Maybe Int)
timeLimitParser = do
    try (do manyTill anyChar (try (string "TM["))
            time <- intParser
            string "]\n"
            return $ Just time)
    <|> return Nothing

-- parsec parser for a gamenode in the sgf file
moveParser :: WorldWithoutGUI -> [WorldWithoutGUI] -> Parsec String () [WorldWithoutGUI]
moveParser w w' = 
    try (do 
        string "(;"
        s <- winParser w
        string ")"
        endOfFile
        return (w' ++ [s]))
    <|> try (do
        string "(;"
        s <- moveParser' w
        string ")\n"
        moveParser s (w' ++ [s]))
    <|> return w'

-- parsec parser for the end of the file
endOfFile :: Parsec String () ()
endOfFile = do
    eof
    return ()

-- parsec parser for a win in the sgf file
winParser :: WorldWithoutGUI -> Parsec String () WorldWithoutGUI
winParser w = 
    try (do string "RE[B+T]"
            let w' = w {state' = Win Black}
            return w')
    <|> (do string "RE[B+R]"
            let w' = w {state' = Win Black}
            return w')
    <|> (do string "RE[B+]"
            let w' = w {state' = Win Black}
            return w')
    <|> (do string "RE[W+T]"
            let w' = w {state' = Win White}
            return w')
    <|> (do string "RE[W+R]"
            let w' = w {state' = Win White}
            return w')
    <|> (do string "RE[W+]"
            let w' = w {state' = Win White}
            return w')
    <|> (do string "RE[0]"
            let w' = w {state' = Draw}
            return w')

-- parsec parser for a move in the sgf file
moveParser' :: WorldWithoutGUI -> Parsec String () WorldWithoutGUI
moveParser' w = try (do moveNotationParser w "W")
    <|> try (do moveNotationParser w "B")
    <|> fail "No move"

-- parsec parser for the notation of a move in the sgf file
moveNotationParser :: WorldWithoutGUI -> String -> Parsec String () WorldWithoutGUI
moveNotationParser w s =
    try (do string (s ++ "[")
            w' <- positionParser w s
            string "]"
            timeRemainingNotationParser w' s)

-- parsec parser for the representation of a position in the sgf file
positionParser :: WorldWithoutGUI -> String -> Parsec String () WorldWithoutGUI
positionParser w s = do
    c1 <- anyChar
    c2 <- anyChar
    return $ positionWithColour w s (convertChar c1, convertChar c2)

positionWithColour :: WorldWithoutGUI -> String -> (Int, Int) -> WorldWithoutGUI
positionWithColour w "B" (x, y) = w { board' = (board' w) { pieces = ((x, (20-y)), Black):pieces (board' w)}}
positionWithColour w "W" (x, y) = w { board' = (board' w) { pieces = ((x, (20-y)), White):pieces (board' w)}}

-- parsec parser for the time remaining notation in the sgf file
timeRemainingNotationParser :: WorldWithoutGUI -> String -> Parsec String () WorldWithoutGUI
timeRemainingNotationParser w s =
    try (do string (s ++ "L[")
            time <- timeRemainingParser w s
            string "]"
            return $ w {clocks' = time})
    <|> return w

-- parsec parser for the time remaining in the sgf file
timeRemainingParser :: WorldWithoutGUI -> String -> Parsec String () (Maybe (Int, Int, Int))
timeRemainingParser w s = do
    time <- intParser
    case clocks' w of
        Just (c1, c2, c3) -> case s of
            "B" -> return $ Just (c1, time, c3)
            "W" -> return $ Just (c1, c2, time)
        Nothing -> return Nothing



-- parsec parser for int
intParser :: Parsec String () Int
intParser = do
  int <- many1 digit
  return (read int)

-- a function to convert numbers to a character
convertNumber :: Int -> Char
convertNumber n' = chr (n' + 96)

-- a function to convert a character to a number
convertChar :: Char -> Int
convertChar c' = ord (toLower c') - 96 
