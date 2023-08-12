module SGF where

import Data.Time.Clock
import Data.Time.Format
import Text.Parsec
import Parser.SGFParser

import Types
import Debug.Trace

rFile = "game.sgf"

getPlayback :: IO (Maybe [WorldWithoutGUI])
getPlayback = do
    file <- readFile rFile
    let parsed = parse sgfParser "game.sgf" file
    case parsed of
        Left err -> trace (show err) return Nothing
        Right x -> return $ Just x

recordGameConfig :: World -> IO ()
recordGameConfig w = do
    writeFile rFile "(;GM[1]FF[4]CA[UTF-8]AP[Gomoku:0.1]\n"
    recordDate
    recordBoardSize w
    recordPlayers w
    recordTimeLimit w
    writeToFile ")\n"

-- Write the date to the file
recordDate :: IO ()
recordDate = do
    currentTime <- getCurrentTime
    let today = utctDay currentTime
    writeToFile $ "DT[" ++ formatTime defaultTimeLocale "%Y-%m-%d" today ++ "]\n"

-- Write the players to the file
recordPlayers :: World -> IO ()
recordPlayers w = writeToFile "PB[Player 1]\nPW[Player 2]\n"

-- Write the time limit to the file if one exists
recordTimeLimit :: World -> IO ()
recordTimeLimit w = do
    case clocks w of
        Nothing -> return ()
        Just (c1,c2,c3) -> do
            writeToFile $ "TM[" ++ show c2 ++ "]\n"

-- Write the board size to the file
recordBoardSize :: World -> IO ()
recordBoardSize w = do
    let s = size $ board w
    writeToFile $ "SZ[" ++ show s ++ "]\n"

-- Write the result of a player move to the file
recordMove :: World -> Col -> Position -> IO ()
recordMove w c p = do
    case state w of
        InPlay -> recordMove' w c p
        Menu -> return ()
        _ -> recordEnd' w c p

-- Write a player move to the file
recordMove' :: World -> Col -> Position -> IO ()
recordMove' w c p = do
    let p' = convertPosition p
    case clocks w of
        Nothing -> case c of
            Black -> writeToFile $ "(;B[" ++ p' ++ "])\n"
            White -> writeToFile $ "(;W[" ++ p' ++ "])\n"
        Just (c1,c2,c3) ->
            case c of
                Black -> writeToFile $ "(;B[" ++ p' ++ "]BL[" ++ show c2 ++ "])\n"
                White -> writeToFile $ "(;W[" ++ p' ++ "]WL[" ++ show c3 ++ "])\n"

-- Convert a position to a sgf position
convertPosition :: Position -> String
convertPosition (x,y) = [convertNumber x, convertNumber (20-y)]

recordEnd' :: World -> Col -> Position -> IO ()
recordEnd' w c p = do
    recordMove' w c p
    recordEnd w c

-- Write the result of the game to the file
recordEnd :: World -> Col -> IO ()
recordEnd w c = do
    case state w of
        Draw -> writeToFile "(;RE[0])"
        Win c -> case c of
            Black -> case flags w of
                Time -> writeToFile "(;RE[B+T])"
                Concede -> writeToFile "(;RE[B+R])"
                _ -> writeToFile "(;RE[B+])"
            White -> case flags w of
                Time -> writeToFile "(;RE[W+T])"
                Concede -> writeToFile "(;RE[W+R])"
                _ -> writeToFile "(;RE[W+])"
        _ -> return ()

-- Append a string to the file
writeToFile :: String -> IO ()
writeToFile = appendFile rFile



