{-|
Module: GUI.Menu.EndScreen
Description: Handles the drawing of the end screen

The 'drawEndScreen' function is used to draw the panel used to display the game result and the different options for the end of the game. It draws the background, the options, and the hover effects.

Using this menu, the user can either start a new game, load a game, or return to the main menu.
-}
module GUI.Menu.EndScreen (
    -- * Drawing the end screen
    drawEndScreen,
    -- * Bounds of the end screen buttons
    endScreenBoundsList,
    endScreenNewGameBounds,
    endScreenLoadGameBounds,
    endScreenReplayBounds,
    endScreenMainBounds
) where

import Graphics.Gloss
    ( black,
      blank,
      color,
      pictures,
      scale,
      text,
      translate,
      makeColor,
      Picture,
      Point )

import Types
    ( World(gui, state),
      Col(White, Black),
      GUI(scaleFactor, hoverPosition),
      GameState(Draw, Win) )
import GUI.Menu.PanelMenu
    ( drawPanel, highlight, convertPointsToTrap )


-- | Used to draw the end screen for different game end options.
drawEndScreen :: World -> Picture
drawEndScreen w =
    case state w of
        Win Black -> drawEndScreen' w "black wins."
        Win White -> drawEndScreen' w "white wins."
        Draw -> drawEndScreen' w "draw."
        _ -> blank

-- Reduces the end screen to a single picture.
drawEndScreen' :: World -> String -> Picture
drawEndScreen' w title = pictures [drawPanel w title 0.3, drawEndScreenHover w, drawForeEndScreen w]

-- Draws the pictures that act as a hover effect on the end screen.
drawEndScreenHover :: World -> Picture
drawEndScreenHover w = do
    case hoverPosition $ gui w of
        Left (-1) -> blank
        -- general hover effect for the buttons
        Left a -> do
            let pic = convertPointsToTrap $ endScreenBoundsList !! a
            scale sf sf $ color highlight pic
        _ -> blank
    where
        sf = scaleFactor $ gui w

-- Used to draw the forground elements of the end screen ie the text.
drawForeEndScreen :: World -> Picture
drawForeEndScreen w =
    pictures [
        scale sf sf $ translate (-160) 10 $ scale 0.2 0.2 $ color black $ text "new game",
        scale sf sf $ translate (-160) (-50) $ scale 0.2 0.2 $ color black $ text "load game",
        scale sf sf $ translate (-160) (-110) $ scale 0.2 0.2 $ color black $ text "replay",
        scale sf sf $ translate (-160) (-170) $ scale 0.2 0.2 $ color black $ text "main menu"
        ]
    where
        sf = scaleFactor $ gui w

-- Defines the boundaries for the different buttons on the end screen.
endScreenBoundsList :: [(Point, Point)]
endScreenBoundsList = [endScreenNewGameBounds, endScreenLoadGameBounds, endScreenReplayBounds, endScreenMainBounds]
endScreenNewGameBounds = ((-165, 0), (0, 40))
endScreenLoadGameBounds = ((-165, -60), (0, -20))
endScreenReplayBounds = ((-165, -120), (0, -80))
endScreenMainBounds = ((-165, -180), (0, -140))