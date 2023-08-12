{-|
Module: GUI.Menu.PanelMenu
Description: Handles the drawing of a background that is used to create a menu panel.

The 'drawPanel' function is used to draw the panel used to display different menus. It draws the background, the panel, and the title of the menu.
-}
module GUI.Menu.PanelMenu (
    -- * Drawing the panel menu background
    drawPanel,
    -- * Standard colour used for hover effects
    highlight,
    -- * Convert two points into trapezium
    convertPointsToTrap,
    -- * Convert two points into rectangle
    convertPointsToRect
) where

import Graphics.Gloss
    ( color,
      pictures,
      polygon,
      rectangleSolid,
      scale,
      text,
      translate,
      makeColor,
      Color,
      Picture,
      Point )

import Types ( World(gui), GUI(scaleFactor, windowResolution) )
import GUI.Background ( drawBackground )


-- | Draws the panel used to display different menus.
drawPanel :: World -> String -> Float -> Picture
drawPanel w title scl = do
    let sf = scaleFactor $ gui w
    pictures [
        drawBackground w,
        color tint $ rectangleSolid (fromIntegral $ fst res) (fromIntegral $ snd res),
        translate (-90*sf) 0 $ color pblue $
            rectangleSolid (fromIntegral (fst res) *0.55) (fromIntegral $ snd res),
        scale sf sf $ translate (-189) 185 $ scale scl scl $ text title
        ]
    where
        tint = makeColor 0.6 0.6 0.6 0.5
        pblue = makeColor 0.566 0.633 0.664 0.9
        res = windowResolution $ gui w

-- | Colour used for hover effects.
highlight :: Color
highlight = makeColor 0.466 0.533 0.564 0.7


-- | Rectangle used for hover effects based on two points.
convertPointsToTrap :: (Point, Point) -> Picture
convertPointsToTrap ((x1,y1), (x2,y2)) = do
    let w = abs (x1-x2)
        h = abs (y1-y2)
    polygon [ (x1 - (w*0.1),y1), (x1,y2), (x2 + (w*0.1),y2), (x2,y1) ]

-- | Trapezium used for hover effect based on two points.
convertPointsToRect :: (Point, Point) -> Picture
convertPointsToRect ((x1,y1), (x2,y2)) = do
    let tr = ( (x1+x2)/2, (y1+y2)/2 )
        w = abs (x1-x2)
        h = abs (y1-y2)
    uncurry translate tr $ rectangleSolid w h
