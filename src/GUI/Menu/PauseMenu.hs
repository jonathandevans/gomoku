{-|
Module: GUI.Menu.PauseMenu
Description: Handles the drawing of the pause menu

The 'drawPauseMenu' function is used to draw the panel used to display different in-game options. It draws the background, the options, and the hover effects.
When active, the pause menu is drawn on top of the game board. However the game elements are hidden.

Using this menu, the user can either resume the game, restart the game, or concede the game.
-}
module GUI.Menu.PauseMenu (
    -- * Drawing the pause menu
    drawPauseMenu,
    -- * Bounds of the pause menu buttons
    pauseMenuBoundsList,
    pauseContinueBounds,
    pauseSaveGameBounds,
    pauseLoadGameBounds,
    pauseHintBounds,
    pauseConcedeBounds
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

import Types ( World(gui), GUI(scaleFactor, hoverPosition) )
import GUI.Menu.PanelMenu
    ( drawPanel, highlight, convertPointsToTrap )


-- | Used to draw the pause menu.
drawPauseMenu :: World -> Picture
drawPauseMenu w =
    pictures [drawPanel w "gomoku" 0.4, drawPauseMenuHover w, drawForePauseMenu w]

-- Draws the pictures that act as a hover effect on the pause menu.
drawPauseMenuHover :: World -> Picture
drawPauseMenuHover w =
    case hoverPosition $ gui w of
        -- no hover effect
        Left (-1) -> blank
        -- special hover effect for the concede button
        Left 4 -> do
            let pic = convertPointsToTrap pauseConcedeBounds
            scale sf sf $ color softred pic
        -- general hover effect for the other buttons
        Left x -> do
            let pic = convertPointsToTrap $ pauseMenuBoundsList !! x
            scale sf sf $ color highlight pic
        _ -> blank
    where
        softred = makeColor 0.858 0.317 0.309 0.5
        sf = scaleFactor $ gui w

-- Used to draw the forground elements of the menu ie the text.
drawForePauseMenu :: World -> Picture
drawForePauseMenu w = do
    let sf = scaleFactor $ gui w
    scale sf sf $ pictures [
        translate (-160) 70 $ scale 0.2 0.2 $ color black $ text "continue",
        translate (-160) 10 $ scale 0.2 0.2 $ color black $ text "save game",
        translate (-160) (-50) $ scale 0.2 0.2 $ color black $ text "load game",
        translate (-160) (-110) $ scale 0.2 0.2 $ color black $ text "toggle hints",
        translate (-160) (-170) $ scale 0.2 0.2 $ color black $ text "concede"
        ]

-- Defines the boundaries for the different buttons on the pause menu.
pauseMenuBoundsList :: [(Point, Point)]
pauseMenuBoundsList = [pauseContinueBounds, pauseSaveGameBounds, pauseLoadGameBounds, pauseHintBounds, pauseConcedeBounds]
pauseContinueBounds = ((-165, 60), (0, 100))
pauseSaveGameBounds = ((-165, 0), (0, 40))
pauseLoadGameBounds = ((-165, -60), (0, -20))
pauseHintBounds = ((-165, -120), (0, -80))
pauseConcedeBounds = ((-165, -180), (0, -140))