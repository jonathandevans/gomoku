{-|
Module: GUI.Menu.ConnectMenu
Description: Handles the drawing of the connect menu

The 'drawConnectMenu' function is used to draw the panel used to allow a user to enter information to connect to a server. It draws the background, the panel, and the title of the menu.

Using this menu, the user can enter a new game started by a server.
-}
module GUI.Menu.ConnectMenu (
    -- * Drawing the connect menu
    drawConnectMenu,
    connectBoundsList,
    connectConnectBounds,
    connectBackBounds
) where

import Graphics.Gloss

import Types
import GUI.Menu.PanelMenu
import Network.BSD (HostEntry(hostName))

-- | Used to draw the connect menu.
drawConnectMenu :: World -> Picture
drawConnectMenu w = pictures [drawPanel w "connect" 0.4, drawBackConnectMenu w, drawConnectMenuHover w, drawForeConnectMenu w]

drawBackConnectMenu :: World -> Picture
drawBackConnectMenu w = scale sf sf $ pictures [
        translate (-170) 40 $ color black $ scale 0.12 0.12 $ text "host:",
        translate (-100) 25 $ color awhite $ rectangleSolid 130 20,
        translate (-150) 0 $ color black $ scale 0.09 0.09 $ text "type to edit host"
        ]
    where
        sf = scaleFactor $ gui w
        awhite = makeColor 0.9 0.9 0.9 0.8

drawConnectMenuHover :: World -> Picture
drawConnectMenuHover w =
    case hoverPosition $ gui w of
        -- special hover effect for the play button
        Left 0 -> do
            let pic = convertPointsToRect $ head connectBoundsList
            scale sf sf $ color pgreen pic
        -- special hover effect for the back button
        Left 1 -> do
            let pic = convertPointsToTrap $ connectBoundsList !! 1
            scale sf sf $ color pred pic
        _ -> blank
    where
        sf = scaleFactor $ gui w
        pgreen = makeColor 0.562 0.711 0.621 0.6
        pred = makeColor 0.862 0.521 0.521 0.5

drawForeConnectMenu :: World -> Picture
drawForeConnectMenu w = scale sf sf $ pictures [
    translate (-190) (-190) $ color black $ scale 0.15 0.15 $ text "<< back",
    translate (-148) (-30) $ color black $ scale 0.15 0.15 $ text "connect >>",
    translate (-150) 19 $ color black $ scale 0.1 0.1 $ text $ host w
    ]
    where
        sf = scaleFactor $ gui w

connectBoundsList :: [(Point, Point)]
connectBoundsList = [connectConnectBounds, connectBackBounds]
connectConnectBounds = ((-192, -40), (7, -9))
connectBackBounds = ((-192, -197),(-104,-168))