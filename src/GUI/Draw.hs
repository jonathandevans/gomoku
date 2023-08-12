{-|
Module: GUI.Draw
Description: Handles the the drawing of the gui.

The 'drawWorld' function is used convert the world state into a picture and render it into the window.

The 'initGUI' function is used to create the basic GUI data type on startup.
This data type is used to store the state of the GUI, such as the current screen, the background picture, and the window resolution.
-}
module GUI.Draw (
    -- * Drawing the world
    drawWorld,
    -- * Instantiating the GUI data type
    initGUI,
    -- * Calculates the space between each line on the grid based on the grid size.
    getGap
) where

import Graphics.Gloss ( Picture(Blank) )
import Graphics.Gloss.Interface.Environment ( getScreenSize )
import qualified Data.Bifunctor as B

import Types
import GUI.Background ( createBackground )
import GUI.Menu.MainMenu ( drawMainMenu )
import GUI.Menu.OptionsMenu ( drawOptionsMenu )
import GUI.Menu.PauseMenu ( drawPauseMenu )
import GUI.Menu.ReplayMenu ( drawReplayMenu )
import GUI.Menu.ConnectMenu ( drawConnectMenu )
import GUI.Menu.WaitingForConnectionMenu
    ( drawWaitingForConnectionMenu )
import GUI.Menu.EndScreen ( drawEndScreen )
import GUI.GameScreen ( drawGame )


-- | Instantiates the GUI data type on startup
initGUI :: IO GUI
initGUI = do
    res <- getWindowRes
    let sf = getScaleFactor res
        br = getBoardRadius sf
        o = getOffset sf
    bak <- createBackground br sf o
    return $ GUI res sf Main br 0 o [] (Left (-1)) (Blank, Blank) bak

-- | Given a world state, return a Picture which will render the world state.
drawWorld :: World -> IO Picture
drawWorld w =
    case currentScreen $ gui w of
        Main -> return $ drawMainMenu w
        Pause -> case state w of
            Playback _ _ -> return $ drawReplayMenu w
            _ -> return $ drawPauseMenu w
        Options -> return $ drawOptionsMenu w
        Connect -> return $ drawConnectMenu w
        WaitingForConnection -> drawWaitingForConnectionMenu w
        GameBoard ->
            case state w of
                InPlay -> return $ drawGame w
                Playback _ _ -> return $ drawGame w
                _ -> return $ drawEndScreen w

-- Sets the resolution of the window using the screen resolution.
getWindowRes :: IO (Int, Int)
getWindowRes = do
    B.bimap (max minWidth) (max minHeight) <$> getScreenRes

-- Constant used to define the minimum width of the window.
minWidth :: Int
minWidth = 441

-- Constant used to define the minimum height of the window.
minHeight :: Int
minHeight = 500

-- Gets the window size using the screen resolution.
getScreenRes :: IO (Int, Int)
getScreenRes = do
    s <- getScreenSize
    if uncurry (>) s
        then return (round $ fromIntegral (snd s) * 0.75, round $ fromIntegral (snd s) * 0.85)
    else
        return (round $ fromIntegral (fst s) * 0.75, round $ fromIntegral (fst s) * 0.85)

-- Used to calculate the factor by which to scale objects.
getScaleFactor :: (Int, Int) -> Float
getScaleFactor res = fromIntegral (fst res)/fromIntegral minWidth

-- Finds the size of the board that exists in the world type.
gridSize :: World -> Int
gridSize w = size (board w)

-- Sets the radius of the board using the width of the window.
getBoardRadius :: Float -> Float
getBoardRadius sf = ((fromIntegral minWidth*0.6)/2)*sf

-- The space between each line on the grid based on the grid size.
getGap :: Float -> Int -> Float
getGap br total = (br*2) / fromIntegral total

-- Used to calculate the offset caused by the scaled window.
getOffset :: Float -> Float
getOffset sf = (-70) * sf
