{-|
Module: GUI.Menu.ReplayMenu
Description: Handles the drawing of the pause menu when replaying a game

The 'drawReplayMenu' function is used to draw the panel used to display different options when replaying a game. It draws the background, the options, and the hover effects.
When active, the menu is drawn on top of the game board. However the game elements are hidden.

Using this menu, the user can either resume the playback, restart the playback, load a game, or exit the playback.
-}
module GUI.Menu.ReplayMenu (
    -- * Drawing the pause menu
    drawReplayMenu,
    -- * Bounds of the pause menu buttons
    replayMenuBoundsList,
    replayContinueBounds,
    replayReplayBounds,
    replayLoadGameBounds,
    replayMainBounds
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
drawReplayMenu :: World -> Picture
drawReplayMenu w =
    pictures [drawPanel w "replay" 0.4, drawReplayMenuHover w, drawForeReplayMenu w]

-- Draws the pictures that act as a hover effect on the pause menu.
drawReplayMenuHover :: World -> Picture
drawReplayMenuHover w =
    case hoverPosition $ gui w of
        -- no hover effect
        Left (-1) -> blank
        -- general hover effect for the other buttons
        Left x -> do
            let pic = convertPointsToTrap $ replayMenuBoundsList !! x
            scale sf sf $ color highlight pic
        _ -> blank
    where
        sf = scaleFactor $ gui w

-- Used to draw the forground elements of the menu ie the text.
drawForeReplayMenu :: World -> Picture
drawForeReplayMenu w = do
    let sf = scaleFactor $ gui w
    scale sf sf $ pictures [
        translate (-160) 70 $ scale 0.2 0.2 $ color black $ text "continue",
        translate (-160) 10 $ scale 0.2 0.2 $ color black $ text "restart",
        translate (-160) (-50) $ scale 0.2 0.2 $ color black $ text "load game",
        translate (-160) (-110) $ scale 0.2 0.2 $ color black $ text "main menu"
        ]

-- Defines the boundaries for the different buttons on the pause menu.
replayMenuBoundsList :: [(Point, Point)]
replayMenuBoundsList = [replayContinueBounds, replayReplayBounds, replayLoadGameBounds, replayMainBounds]
replayContinueBounds = ((-165, 60), (0, 100))
replayReplayBounds = ((-165, 0), (0, 40))
replayLoadGameBounds = ((-165, -60), (0, -20))
replayMainBounds = ((-165, -120), (0, -80))