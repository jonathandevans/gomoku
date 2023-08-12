{-|
Module: GUI.Input
Description: Handles input for the game

The 'handleInput' function is used to handle input events. It is used to update the world state after an input event.

If the game us in singleplayer mode, the 'handleInput' function is used to update the board state when the player makes a move.

If the game is in multiplayer mode, the 'handleInput' function is used to send messages to the other player when the player makes a move.
Server side message receiving is handled in the 'updateWorld' function in the 'AI' module.
-}
module GUI.Input (
    -- * Input handling for gui interactions
    handleInput
) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Data.Maybe

import Types
import Board
import GUI.Menu.MainMenu
import GUI.Menu.PauseMenu
import GUI.Menu.OptionsMenu
import GUI.Menu.EndScreen
import GUI.Menu.ReplayMenu
import GUI.Menu.WaitingForConnectionMenu
import GUI.Menu.ConnectMenu
import System.Exit
import Protocol
import SGF


-- | Update the world state given an input event.
handleInput :: Event -- ^ The input event
            -> World -- ^ The world state
            -> IO World -- ^ The updated world state

-- Update the mouse position on movement to allow for hover effects
handleInput (EventMotion (x,y)) w
    | currentScreen (gui w) == Main = handleMenuMovement w (x, y) mainMenuBoundsList
    | currentScreen (gui w) == Pause =
        case state w of
            Playback _ _ -> handleMenuMovement w (x, y) replayMenuBoundsList
            _ -> handleMenuMovement w (x, y) pauseMenuBoundsList
    | currentScreen (gui w) == Options = handleMenuMovement w (x, y) $ optionsMenuBoundsList w
    | currentScreen (gui w) == Connect = handleMenuMovement w (x, y) connectBoundsList
    | currentScreen (gui w) == WaitingForConnection = handleMenuMovement w (x, y) waitingBoundsList
    | currentScreen (gui w) == GameBoard =
        case state w of
            InPlay -> handleGameMovement w (x,y)
            _ -> handleMenuMovement w (x, y) endScreenBoundsList

-- When the left mouse button is pressed, save the current board state to previousBoards then update the board 
-- If in the pause menu, save or load the game depending on where the mouse was clicked
handleInput (EventKey (MouseButton LeftButton) Up _ (x, y)) w
    | currentScreen (gui w) == Main = handleMainMenuClick w (x, y)
    | currentScreen (gui w) == Options = handleOptionsMenuClick w (x, y)
    | currentScreen (gui w) == Pause =
        case state w of
            Playback _ _ -> handleReplayMenuClick w (x, y)
            _ -> handlePauseMenuClick w (x, y)
    | currentScreen (gui w) == Connect = handleConnectClick w (x, y)
    | currentScreen (gui w) == WaitingForConnection = handleWaitingForConnectionClick w (x, y)
    | currentScreen (gui w) == GameBoard =
        case state w of
            InPlay -> handleGameClick w (x,y)
            _ -> handleEndScreenClick w (x,y)

-- When z is pressed, undo the last move
handleInput (EventKey (Char 'z') Up m (x, y)) w
    | currentScreen (gui w) == GameBoard =
        return $ undoMove w
    | otherwise = return w

-- When the p key is pressed, toggle pause menu
handleInput (EventKey (Char 'p') Up m (x, y)) w = do
    case state w of
        Playback l c -> case currentScreen $ gui w of
            GameBoard -> return $ w {gui = (gui w) {currentScreen = Pause}, state = Playback l c}
            Pause -> return $ w {gui = (gui w) {currentScreen = GameBoard}, state = Playback l c}
            _ -> return w
        InPlay -> case currentScreen $ gui w of
            GameBoard -> return $ w {gui = (gui w) {currentScreen = Pause}, state = if gameMode w == Online then InPlay else Menu}
            Pause -> return $ w {gui = (gui w) {currentScreen = GameBoard}, state = InPlay}
            _ -> return w
        Menu -> case currentScreen $ gui w of
            GameBoard -> return $ w {gui = (gui w) {currentScreen = Pause}, state = Menu}
            Pause -> return $ w {gui = (gui w) {currentScreen = GameBoard}, state = InPlay}
            _ -> return w
        _ -> return w


handleInput (EventKey (Char '\b') Up m (x, y)) w
    | currentScreen (gui w) == Connect =
        return $ w {host = removeChar $ host w}

handleInput (EventKey (Char c) Up m (x, y)) w
    | currentScreen (gui w) == Connect =
        return $ w {host = host w ++ [c]}

handleInput e w = return w


removeChar :: String -> String
removeChar [] = []
removeChar [x] = []
removeChar (x:xs) = x : removeChar xs

-- Update the world state on the movement of the mouse in a menu
handleMenuMovement :: World -> Point -> [(Point, Point)] -> IO World
handleMenuMovement w a b =
    return $ w {gui = (gui w) {hoverPosition = Left $ checkBounds a sf b 0}}
    where
        sf = scaleFactor $ gui w

-- Update the world state on the movement of the mouse in the game board
handleGameMovement :: World -> Point -> IO World
handleGameMovement w (x,y) =
    return w { gui = (gui w){ hoverPosition = Right $ findGridPosition w (x, y) }}


-- Update the world state on the click of the mouse in the main menu
handleMainMenuClick :: World -> Point -> IO World
handleMainMenuClick w (x,y)
    | withinBounds (x,y) mainNewGameBounds sf =
        return w { state = Menu, gui = (gui w) {currentScreen = Options, hoverPosition=Left (-1)} }
    | withinBounds (x,y) mainLoadGameBounds sf = do
        loadGame w { state = InPlay, gui = (gui w) {currentScreen = GameBoard, hoverPosition=Left (-1)} }
    | withinBounds (x,y) mainConnectBounds sf =
        return w { state = Menu, gui = (gui w) {currentScreen = Connect, hoverPosition=Left (-1)} }
    | withinBounds (x,y) mainQuitBounds sf = do
        exitSuccess
    | otherwise = return w
    where
        sf = scaleFactor $ gui w

-- Update the world state on the click of the mouse in the options menu
handleOptionsMenuClick :: World -> Point -> IO World
handleOptionsMenuClick w (x,y)
    | withinBounds (x,y) optionsIncrBoardSizeBoundaries sf = do
        let s = limitBoardSize $ size (board w) + 1
            t = limitTarget (target $ board w) s
        return  w { board = (board w) { size = s, target =  t} }
    | withinBounds (x,y) optionsDecrBoardSizeBoundaries sf = do
        let s = limitBoardSize $ size (board w) - 1
            t = limitTarget (target $ board w) s
        return w { board = (board w) { size = s, target = t } }
    | withinBounds (x,y) optionsIncrTargetBoundaries sf = do
        let t = limitTarget (target (board w) + 1) (size $ board w)
        return w { board = (board w) { target = t } }
    | withinBounds (x,y) optionsDecrTargetBoundaries sf = do
        let t = limitTarget (target (board w) - 1) (size $ board w)
        return w { board = (board w) { target = t } }
    | withinBounds (x,y) optionsIncrTimerBoundaries sf = do
        case limitTimer (clocks w) True of
            Nothing -> return w { clocks = Nothing }
            Just t -> return w { clocks = Just (0,t,t) }
    | withinBounds (x,y) optionsDecrTimerBoundaries sf = do
        case limitTimer (clocks w) False of
            Nothing -> return w { clocks = Nothing }
            Just t -> return w { clocks = Just (0,t,t) }
    | withinBounds (x,y) optionsIncrGameModeBoundaries sf = do
        return w { gameMode = nextGameMode (gameMode w) }
    | withinBounds (x,y) optionsDecrGameModeBoundaries sf = do
        return w { gameMode = previousGameMode (gameMode w) }
    | withinBounds (x,y) optionsIncrColourBoundaries sf = do
        let c = other (turn w)
        return w { turn = c }
    | withinBounds (x,y) optionsDecrColourBoundaries sf = do
        let c = other (turn w)
        return w { turn = c }
    | withinBounds (x,y) optionsPlayBoundaries sf = pressedPlay w
    | withinBounds (x,y) optionsBackBoundaries sf = do
        return w { state = Menu, gui = (gui w) {currentScreen = Main, hoverPosition = Left (-1)} }
    | otherwise = return w
    where
        sf = scaleFactor $ gui w

-- Update the world state when the play button is pressed in the options menu
pressedPlay :: World -> IO World
pressedPlay w = do
    recordGameConfig w
    case gameMode w of
        Online ->
            return w { state = Menu, gameMode = Local, gui = (gui w) {currentScreen = WaitingForConnection, hoverPosition = Left (-1)} }
        _ ->
            initWorld (InPlay, size $ board w, target $ board w, gameMode w, turn w, "", clocks w)
                (gui (w { gui = (gui w) {currentScreen = GameBoard, hoverPosition = Left (-1) } }))

handleWaitingForConnectionClick :: World -> Point -> IO World
handleWaitingForConnectionClick w (x,y)
    | withinBounds (x,y) waitingOpenBounds sf = do
        initWorld (InPlay, size $ board w, target $ board w, Online, White, "", clocks w)
            (gui (w { gui = (gui w) {currentScreen = GameBoard, hoverPosition = Left (-1) } }))
    | withinBounds (x,y) waitingBackBounds sf = do
        return w { state = Menu, gui = (gui w) {currentScreen = Options, hoverPosition = Left (-1)} }
    | otherwise = return w
    where
        sf = scaleFactor $ gui w

handleReplayMenuClick :: World -> Point -> IO World
handleReplayMenuClick w (x,y)
    | withinBounds (x,y) replayContinueBounds sf = do
        return w { gui = (gui w) {currentScreen = GameBoard, hoverPosition = Left (-1)} }
    | withinBounds (x,y) replayReplayBounds sf = do
        p <- getPlayback
        case p of
            Just p' -> do
                printL p'
                return w { state = Playback p' 0, board  = (board w) {pieces = []}, gui = (gui w) {currentScreen = GameBoard, hoverPosition = Left (-1)} }
            Nothing -> return w
    | withinBounds (x,y) replayLoadGameBounds sf = do
        loadGame w { state = InPlay, gui = (gui w) {currentScreen = GameBoard, hoverPosition=Left (-1)} }
    | withinBounds (x,y) replayMainBounds sf = do
        return w { state = Menu, gui = (gui w) {currentScreen = Main, hoverPosition = Left (-1)} }
    | otherwise = return w
    where
        sf = scaleFactor $ gui w

handleConnectClick :: World -> Point -> IO World
handleConnectClick w (x,y)
    | withinBounds (x,y) connectBackBounds sf = do
        return w { state = Menu, gui = (gui w) {currentScreen = Main, hoverPosition = Left (-1)} }
    | withinBounds (x,y) connectConnectBounds sf = do
        initWorld (InPlay, size $ board w, target $ board w, Online, Black, host w, clocks w)
            (gui (w { gui = (gui w) {currentScreen = GameBoard, hoverPosition = Left (-1) } }))
    | otherwise = return w
    where
        sf = scaleFactor $ gui w

-- Handles the click event when on the pause screen
handlePauseMenuClick :: World -> Point -> IO World
handlePauseMenuClick w (x,y)
    | withinBounds (x,y) pauseContinueBounds sf = do
        return $ w {state = InPlay, gui = (gui w) {currentScreen = GameBoard, hoverPosition = Left (-1)}}
    | withinBounds (x,y) pauseSaveGameBounds sf = do
        if gameMode w == Online then return w
        else do
          saveGame w
          return $ w {gui = (gui w) {currentScreen = Pause}}
    | withinBounds (x,y) pauseLoadGameBounds sf = do
        if gameMode w == Online then return w
        else do
          loadGame w { state = InPlay, gui = (gui w) {currentScreen = GameBoard} }
    | withinBounds (x,y) pauseConcedeBounds sf = do
        if gameMode w == Online then do
            if local_online w == "Local" then do
                writeMessage (handle w) (worldWithoutGUIToByteString (worldToWorldWithoutGUI w {state = Win (other (turn w))}))
                recordEnd w {state = Win (other (turn w)), flags = Concede} (other (turn w))
                return $ w {state = Win (other (turn w)), gui = (gui w) {currentScreen = GameBoard, hoverPosition = Left (-1)}}
            else do
                writeMessage (handle w) (worldWithoutGUIToByteString (worldToWorldWithoutGUI w {state = Win (turn w)}))
                recordEnd w {state = Win (turn w), flags = Concede} (turn w)
                return $ w {state = Win (turn w), gui = (gui w) {currentScreen = GameBoard, hoverPosition = Left (-1)}}
        else do
            recordEnd w {state = Win (other (turn w)), flags = Concede} (other (turn w))
            return $ w {state = Win (other (turn w)), gui = (gui w) {currentScreen = GameBoard, hoverPosition = Left (-1)}}
    | otherwise = return w
    where
        sf = scaleFactor $ gui w

-- Handles the click event when playing the game
handleGameClick :: World -> Point -> IO World
handleGameClick w (x,y) = do
    if gameMode w == Online then
        if local_online w == "Local" then do
            let pos = findGridPosition w (x, y)
                b' = board w
                in case makeMove (board w) (turn w) pos of
                    Just b -> do
                        recordMove w (turn w) pos
                        if makeWon b == Just (turn w) then do
                            recordEnd w {state = Win (turn w)} (turn w)
                            let w' = w {state = Win (turn w)}
                            writeMessage (handle w) (worldWithoutGUIToByteString (worldToWorldWithoutGUI w'))
                            return w'
                        else if makeWon b == Just (other (turn w)) then do
                            recordEnd w {state = Win (other (turn w))} (other (turn w))
                            let w' = w {state = Win (other (turn w))}
                            writeMessage (handle w) (worldWithoutGUIToByteString (worldToWorldWithoutGUI w'))
                            return w'
                        else
                            do
                            let w' = World InPlay b (gameMode w) (other (turn w)) (winner w) (local_online w) (host w) (b':previousBoards w) (clocks w) (socket w) (handle w) (flags w) (gui w)
                            writeMessage (handle w) (worldWithoutGUIToByteString (worldToWorldWithoutGUI w'))
                            let w'' = World InPlay b (gameMode w) (other (turn w)) (winner w) "Remote" (host w) (b':previousBoards w) (clocks w) (socket w) (handle w) (flags w) (gui w)
                            return w''
                    Nothing -> return w
        else return w
    else do
        let b' = board w
            pos = findGridPosition w (x, y)
        case makeMove (board w) (turn w) pos of
            Just b -> do
                recordMove w (turn w) pos
                if makeWon b == Just (turn w) then do
                    recordEnd w {state = Win (turn w)} (turn w)
                    return w {board = b, turn = other (turn w), previousBoards = b':previousBoards w, state = Win (turn w)}
                else (if (makeWon b == Just (other (turn w))) || (clocks w == Just (0, 0, 0)) then (do
                    recordEnd w {state = Win (other (turn w))} (other (turn w))
                    return w {board = b, turn = other (turn w), previousBoards = b':previousBoards w, state = Win (other (turn w))}) else return w {board = b, turn = other (turn w), previousBoards = b':previousBoards w})
            Nothing -> return w

checkWon :: World -> IO World
checkWon w = if checkBoardEmpty w then return w
    else
    case makeWon (board w) of
    Just c -> return w {state = Win c}
    Nothing -> if clocks w == Just (0, 0, 0) then
                    return w {state = Win (other (turn w))}
                else return w

checkBoardEmpty :: World -> Bool
checkBoardEmpty w = case pieces (board w) of
    [] -> True
    _ -> False


-- Handles the click event when on the end screen
handleEndScreenClick :: World -> Point -> IO World
handleEndScreenClick w pos
    | withinBounds pos endScreenNewGameBounds sf = do
        return w { state = Menu, gui = (gui w) {currentScreen = Options, hoverPosition=Left (-1)} }
    | withinBounds pos endScreenLoadGameBounds sf = do
        loadGame w { state = Menu, gui = (gui w) {hoverPosition = Left (-1)} }
    | withinBounds pos endScreenReplayBounds sf = do
        p <- getPlayback
        case p of
            Just p' -> do
                printL p'
                return w { state = Playback p' 0, board  = (board w) {pieces = []}, gui = (gui w) {currentScreen = GameBoard, hoverPosition = Left (-1)} }
            Nothing -> return w
    | withinBounds pos endScreenMainBounds sf = do
        return $ w {state = Menu, gui = (gui w) {currentScreen = Main, hoverPosition = Left (-1)}}
    | otherwise = return w
    where
        sf = scaleFactor $ gui w

printL :: [WorldWithoutGUI] -> IO ()
printL [] = return ()
printL (x:xs) = do
    print x
    printL xs

-- Checks whether a point exists within a list of boundaries
checkBounds :: Point -> Float -> [(Point, Point)] -> Int -> Int
checkBounds _ _ [] _ = -1
checkBounds a sf (x:xs) c   | withinBounds a x sf = c
                            | otherwise = checkBounds a sf xs (c+1)

-- Checks whether a point exists within a boundary
withinBounds :: Point -> (Point, Point) -> Float -> Bool
withinBounds (x', y') ((x1, y1), (x2, y2)) sf =
    (x1*sf) <= x' && x' <= (x2*sf) && (y1*sf) <= y' && y' <= (y2*sf)

-- Converts the tuple of coordinates into a game board grid position.
findGridPosition :: World -> (Float, Float) -> Position
findGridPosition w (x, y) = do
    let lns = gridLines $ gui w
        o = offset $ gui w
        g = gap $ gui w
    (findGridPositionInList w lns x 0 (size $ board w) o g,
     findGridPositionInList w lns y 0 (size $ board w) 0 g)

-- Finds whether the float exists near a value in array of floats, returning the index of float it is near.
findGridPositionInList :: World -> [Float] -> Float -> Int -> Int -> Float -> Float -> Int
findGridPositionInList _ [] _ _ _ _ _ = -1
findGridPositionInList w (p:ps) coor count total o g
    | coor >= p-(g/2)+o && coor <= p+(g/2)+o = count+1
    | otherwise = findGridPositionInList w ps coor (count+1) total o g

-- Returns the bounds of the grid limiting the size of the board to 6x6 and 19x19
limitBoardSize :: Int -> Int
limitBoardSize x
    | x < 6 = 19
    | x > 19 = 6
    | otherwise = x

-- Returns the bounds of the target limiting the target to 2 and the size of the board
limitTarget :: Int -> Int -> Int
limitTarget x y
    | x < 3 = y-1
    | x == y = y-1
    | otherwise = x

-- Returns the bounds of the timer limiting the timer to 5 minutes and 20 minutes, or turning it off
limitTimer :: Maybe (Int,Int,Int) -> Bool -> Maybe Int
limitTimer (Just (c,c1,c2)) True = if c1+60 > 1200 then Nothing else Just (c1+60)
limitTimer Nothing True = Just 300
limitTimer (Just (c,c1,c2)) False = if c1-60 < 300 then Nothing else Just (c1-60)
limitTimer Nothing False = Just 1200
