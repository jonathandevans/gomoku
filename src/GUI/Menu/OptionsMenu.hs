{-|
Module: GUI.Menu.OptionsMenu
Description: Handles the drawing of the options menu

The 'drawOptionsMenu' function is used to draw the panel used to display different game options. It draws the background, the options, and the hover effects.

If the gamemode is set to local, the options displayed include the board size, target, timer, and game mode.

If the gamemode is in online mode, the colour of the player is displayed as white.
If the gamemode is in AI mode, the colour of the player is given the option to be either white or black.
-}
module GUI.Menu.OptionsMenu (
    -- * Drawing the options menu
    drawOptionsMenu,
    -- * Bounds of the options menu buttons
    optionsMenuBoundsList,
    optionsIncrBoardSizeBoundaries,
    optionsDecrBoardSizeBoundaries,
    optionsIncrTargetBoundaries,
    optionsDecrTargetBoundaries,
    optionsIncrTimerBoundaries,
    optionsDecrTimerBoundaries,
    optionsIncrGameModeBoundaries,
    optionsDecrGameModeBoundaries,
    optionsIncrColourBoundaries,
    optionsDecrColourBoundaries,
    optionsBackBoundaries,
    optionsPlayBoundaries
) where

import Graphics.Gloss
    ( black,
      blank,
      color,
      pictures,
      rectangleSolid,
      scale,
      text,
      translate,
      makeColor,
      Picture,
      Point )

import Types
    ( World(gameMode, board, clocks, turn, gui),
      Board(target, size),
      GUI(scaleFactor, hoverPosition),
      GameMode(AI, Online) )
import GUI.Background ( drawBackground )
import GUI.Menu.PanelMenu
    ( drawPanel, convertPointsToTrap, convertPointsToRect ) 


-- | Used to draw the options menu.
drawOptionsMenu :: World -> Picture
drawOptionsMenu w = pictures [drawPanel w "options" 0.4, drawBackOptionsMenu w, drawOptionsMenuHover w, drawForeOptionsMenu w]

-- Used to draw the background elements of the options menu.
drawBackOptionsMenu :: World -> Picture
drawBackOptionsMenu w = scale sf sf $ pictures [
    boardSelector w,
    targetSelector w,
    timerSelector w,
    gamemodeSelector w,
    -- only draw the colour selector if the game mode is AI
    if gameMode w == AI then colourSelector w else blank,
    -- only draw the colour warning if the game mode is online
    if gameMode w == Online then colourWarning else blank
    ]
    where
        sf = scaleFactor $ gui w

-- Used to draw the elements that allow the user to select the board size.
boardSelector :: World -> Picture
boardSelector w = pictures [
    translate (-170) 140 $ color black $ scale 0.12 0.12 $ text "board size",
    translate (-100) 125 $ color awhite $ rectangleSolid 130 20,
    translate (-150) 119 $ color black $ scale 0.12 0.12 $ text (show $ size $ board w),
    translate (-20) 135 $ color awhite $ rectangleSolid 15 15,
    translate (-20) 115 $ color awhite $ rectangleSolid 15 15,
    translate (-25) 131 $ color black $ scale 0.1 0.1 $ text "+",
    translate (-25) 111 $ color black $ scale 0.1 0.1 $ text "-"
    ]
    where
        awhite = makeColor 0.9 0.9 0.9 0.8

-- Used to draw the elements that allow the user to select the target size.
targetSelector :: World -> Picture
targetSelector w = pictures [
    translate (-170) 80 $ color black $ scale 0.12 0.12 $ text "target",
    translate (-100) 65 $ color awhite $ rectangleSolid 130 20,
    translate (-150) 59 $ color black $ scale 0.12 0.12 $ text (show $ target $ board w),
    translate (-20) 75 $ color awhite $ rectangleSolid 15 15,
    translate (-20) 55 $ color awhite $ rectangleSolid 15 15,
    translate (-25) 71 $ color black $ scale 0.1 0.1 $ text "+",
    translate (-25) 51 $ color black $ scale 0.1 0.1 $ text "-"
    ]
    where
        awhite = makeColor 0.9 0.9 0.9 0.8

-- Used to draw the elements that allow the user to select the timer length.
timerSelector :: World -> Picture
timerSelector w = pictures [
    translate (-170) 20 $ color black $ scale 0.12 0.12 $ text "timer",
    translate (-100) 5 $ color awhite $ rectangleSolid 130 20,
    translate (-150) (-1) $ color black $ scale 0.12 0.12 $ text (getTimer w),
    translate (-20) 15 $ color awhite $ rectangleSolid 15 15,
    translate (-20) (-5) $ color awhite $ rectangleSolid 15 15,
    translate (-25) 11 $ color black $ scale 0.065 0.065 $ text "/\\",
    translate (-25) (-9) $ color black $ scale 0.065 0.065 $ text "\\/"
    ]
    where
        awhite = makeColor 0.9 0.9 0.9 0.8

-- Converts the game clock type into a suitable string format. 
getTimer :: World -> String
getTimer w = case clocks w of
    Just (c1,c2,c3) -> show (round (fromIntegral c2/60)) ++ " mins"
    Nothing -> "Off"

-- Used to draw the elements that allow the user to select the gamemode.
gamemodeSelector :: World -> Picture
gamemodeSelector w = pictures [
    translate (-170) (-40) $ color black $ scale 0.12 0.12 $ text "gamemode",
    translate (-100) (-55) $ color awhite $ rectangleSolid 130 20,
    translate (-150) (-61) $ color black $ scale 0.12 0.12 $ text (show $ gameMode w),
    translate (-20) (-45) $ color awhite $ rectangleSolid 15 15,
    translate (-20) (-65) $ color awhite $ rectangleSolid 15 15,
    translate (-25) (-49) $ color black $ scale 0.065 0.065 $ text "/\\",
    translate (-25) (-69) $ color black $ scale 0.065 0.065 $ text "\\/"
    ]
    where
        awhite = makeColor 0.9 0.9 0.9 0.8

-- Used to draw the elements that allow the user to select the colour.
colourSelector :: World -> Picture
colourSelector w = pictures [
    translate (-170) (-100) $ color black $ scale 0.12 0.12 $ text "your colour",
    translate (-100) (-115) $ color awhite $ rectangleSolid 130 20,
    translate (-150) (-121) $ color black $ scale 0.12 0.12 $ text (show $ turn w),
    translate (-20) (-105) $ color awhite $ rectangleSolid 15 15,
    translate (-20) (-125) $ color awhite $ rectangleSolid 15 15,
    translate (-25) (-109) $ color black $ scale 0.065 0.065 $ text "/\\",
    translate (-25) (-129) $ color black $ scale 0.065 0.065 $ text "\\/"
    ]
    where
        awhite = makeColor 0.9 0.9 0.9 0.8

-- Used to draw the warning that appears when the user selects the online gamemode.
colourWarning :: Picture
colourWarning = pictures [
    translate (-170) (-100) $ color black $ scale 0.12 0.12 $ text "your colour",
    translate (-100) (-115) $ color awhite $ rectangleSolid 130 20,
    translate (-150) (-121) $ color black $ scale 0.12 0.12 $ text "White"
    ]
    where
        awhite = makeColor 0.7 0.7 0.7 0.8

-- Draws the pictures that act as a hover effect on the options menu.
drawOptionsMenuHover :: World -> Picture
drawOptionsMenuHover w =
    case hoverPosition $ gui w of
        -- no hover effect
        Left (-1) -> blank
        -- special hover effect for the play button
        Left 0 -> do
            let pic = convertPointsToTrap $ head (optionsMenuBoundsList w)
            scale sf sf $ color pgreen pic
        -- special hover effect for the back button
        Left 1 -> do
            let pic = convertPointsToTrap $ optionsMenuBoundsList w !! 1
            scale sf sf $ color pred pic
        -- general hover effect for the other buttons
        Left a -> do
            let pic = convertPointsToRect $ optionsMenuBoundsList w !! a
            scale sf sf $ color pblue pic
        _ -> blank
    where
        sf = scaleFactor $ gui w
        pblue = makeColor 0.662 0.721 0.811 0.5
        pgreen = makeColor 0.562 0.711 0.621 0.8
        pred = makeColor 0.862 0.521 0.521 0.8

-- Used to draw the forground elements of the options menu ie the text.
drawForeOptionsMenu :: World -> Picture
drawForeOptionsMenu w = scale sf sf $ pictures [
    translate (-68) (-190) $ color black $ scale 0.15 0.15 $ text "play >>",
    translate (-190) (-190) $ color black $ scale 0.15 0.15 $ text "<< back"
    ]
    where
        sf = scaleFactor $ gui w

-- Defines the boundaries for the different buttons on the options menu.
optionsMenuBoundsList :: World -> [(Point, Point)]
optionsMenuBoundsList w = [ optionsPlayBoundaries, optionsBackBoundaries,
                            optionsIncrBoardSizeBoundaries, optionsDecrBoardSizeBoundaries,
                            optionsIncrTargetBoundaries, optionsDecrTargetBoundaries,
                            optionsIncrTimerBoundaries, optionsDecrTimerBoundaries,
                            optionsIncrGameModeBoundaries, optionsDecrGameModeBoundaries ] ++
                            if gameMode w == AI then [optionsIncrColourBoundaries, optionsDecrColourBoundaries] else []
optionsIncrBoardSizeBoundaries = ((-27.5,127.5), (-12.5,142.5))
optionsDecrBoardSizeBoundaries = ((-27.5,107.5), (-12.5,122.5))
optionsIncrTargetBoundaries = ((-27.5, 67.5),(-12.5,82.5))
optionsDecrTargetBoundaries = ((-27.5, 47.5),(-12.5,62.5))
optionsIncrTimerBoundaries = ((-27.5, 7.5),(-12.5,22.5))
optionsDecrTimerBoundaries = ((-27.5, -12.5),(-12.5,2.5))
optionsIncrGameModeBoundaries = ((-27.5, -52.5),(-12.5,-37.5))
optionsDecrGameModeBoundaries = ((-27.5, -72.5),(-12.5,-57.5))
optionsIncrColourBoundaries = ((-27.5, -112.5),(-12.5,-97.5))
optionsDecrColourBoundaries = ((-27.5, -132.5),(-12.5,-117.5))
optionsPlayBoundaries = ((-75, -197),(10,-168))
optionsBackBoundaries = ((-192, -197),(-104,-168))