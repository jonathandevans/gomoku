{-|
Module: Main
Description: The main module for the Gomoku application.

The main module for the Gomoku application. This module is responsible for starting the application.
-}
module Main(
  main
) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Environment
import Text.Parsec
import Network.Socket

import Board
import GUI.Draw
import GUI.Input
import AI
import Types
import Protocol
import Parser.ArgumentParser


-- | Starts the program.
main :: IO ()
main = do
    start <- getArguments
    g <- initGUI
    openWindow $ initWorld start g


-- Opens a new window relative to the screen resolution.
-- The world state is passed in, with drawWorld used to render a picure when the world updates.
-- handleInput is used for event handling.
-- updateWorld is called 10 times per second.
openWindow :: IO World -> IO()
openWindow world = do
    w <- world
    playIO (InWindow "gomoku" (windowResolution $ gui w) (10, 10)) pblue 10
        w
        drawWorld -- in Draw.hs
        handleInput -- in Input.hs
        updateWorld -- in AI.hs
    where
        pblue = makeColor 0.666 0.733 0.764 1

-- Checks whether arguments have been given, and parses them for the appropriate type.
-- Whilst not all arguments are required, they must be in order.
-- Size, goal, gamemode, starting colour.
getArguments :: IO (GameState, Int, Int, GameMode, Col, HostName, Maybe (Int,Int,Int))
getArguments = do
    args <- getArgs
    let args' = foldr (\x y -> x ++ " " ++ y) "" args
        default' = (Menu, 19, 5, Local, Black, "localhost", Just (0,600,600))

    case parse (argumentParser default') "arguments" args' of
        Left err -> return default'
        Right x -> return x
