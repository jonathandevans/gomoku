{-|
Module: GUI.Menu.MainMenu
Description: Handles the drawing of the main menu

The 'drawMainMenu' function is used to draw the main menu. It draw the background, hover, and foreground elements of the main menu when the world state updates.
-}
module GUI.Menu.MainMenu (
    -- * Drawing the main menu
    drawMainMenu,
    -- * Bounds of the main menu buttons
    mainMenuBoundsList,
    mainNewGameBounds,
    mainLoadGameBounds,
    mainConnectBounds,
    mainQuitBounds
) where

import Graphics.Gloss
    ( black,
      blank,
      color,
      pictures,
      rectangleSolid,
      rectangleWire,
      scale,
      text,
      translate,
      makeColor,
      Picture,
      Point )

import Types
    ( World(gui), GUI(piecesPictures, scaleFactor, hoverPosition) )
import GUI.Menu.PanelMenu ( convertPointsToRect )


-- | Draws the main menu.
drawMainMenu :: World -> Picture
drawMainMenu w = do
    let sf = scaleFactor $ gui w
    scale sf sf $ pictures [drawBackMainMenu, drawMainMenuHover w, drawForeMainMenu]

-- Draws the background elements of the main menu.
drawBackMainMenu :: Picture
drawBackMainMenu = pictures
    [translate (-120) 100 $ scale 0.5 0.5 $ color black $ text "gomoku",
        -- draw box around new game
        translate 0 (-20) $ color grey $ rectangleWire 185 35,
        -- draw box around load game
        translate 0 (-70) $ color grey $ rectangleWire 185 35,
        -- draw box around connect
        translate 0 (-120) $ color grey $ rectangleWire 185 35,
        -- draw box around quit
        translate 0 (-170) $ color grey $ rectangleWire 145 30
        ]
    where
        grey = makeColor 0.3 0.3 0.3 0.7

-- Draws the pictures that act as a hover effect on the main menu.
drawMainMenuHover :: World -> Picture
drawMainMenuHover w = 
    case hoverPosition $ gui w of
        -- no hover effect
        Left (-1) -> blank
        -- draw hover effect
        Left x -> hoverEffect w (even x) $ mainMenuBoundsList !! x
        _ -> blank
    where
        highlight = makeColor 0.466 0.533 0.564 1

-- Draws the foreground elements of the main menu.
drawForeMainMenu :: Picture
drawForeMainMenu = pictures 
    [translate (-50) (-28) $ scale 0.15 0.15 $ color black $ text "new game",
        translate (-53) (-78) $ scale 0.15 0.15 $ color black $ text "load game",
        translate (-33) (-128) $ scale 0.15 0.15 $ color black $ text "connect",
        translate (-14) (-178) $ scale 0.13 0.13 $ color black $ text "quit"
        ]

-- Defines the hover effect for the main menu.
-- Adds a dark overlay to the selected menu item and draws a piece in the corner.
hoverEffect :: World -> Bool -> (Point,Point) -> Picture
hoverEffect w b p =
    color highlight $ pictures [convertPointsToRect p, pieceInCorner w p b]
    where
        highlight = makeColor 0.466 0.533 0.564 1

-- Draws a piece in the corner of a rectangle.
pieceInCorner :: World -> (Point, Point) -> Bool -> Picture
pieceInCorner w ((x1,y1), (x2,y2)) b = do
    let ps = piecesPictures $ gui w
        p = if b then fst ps else snd ps
    translate x2 y2 $ scale 0.6 0.6 p

-- Defines the bounds of the main menu buttons.
mainMenuBoundsList :: [(Point, Point)]
mainMenuBoundsList = [mainNewGameBounds, mainLoadGameBounds, mainConnectBounds, mainQuitBounds]
mainNewGameBounds = ((-92.5, -37.5), (92.5, -2.5))
mainLoadGameBounds = ((-92.5, -87.5), (92.5, -52.5))
mainConnectBounds = ((-92.5, -137.5), (92.5, -102.5))
mainQuitBounds = ((-72.5, -185), (72.5, -155))
