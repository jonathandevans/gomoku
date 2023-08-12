{-|
Module: GUI.GameScreen
Description: Handles the the drawing of the game elements in the gui.

The 'drawGame' function is used convert the game elements into a picture and render it into the window.
THis includes the background, the grid, the pieces, and the hover.

The 'createPieces' function is used to create the basic pieces pictures on startup of a game.

The 'findGridLines' function is used to create the array of floats that represent the distance of a line from the centre of the gui.
-}
module GUI.GameScreen (
    -- * Drawing the game
    drawGame,
    -- * Imports and scales the pieces from a bmp file
    createPieces,
    -- * Creates and array of floats that represent the distance of a line from the centre of the gui
    findGridLines
) where

import Graphics.Gloss
    ( Picture,
      black,
      white,
      blank,
      circleSolid,
      color,
      line,
      pictures,
      scale,
      text,
      translate,
      makeColor,
      bitmapDataOfBMP,
      bitmapOfBMP,
      Color,
      BitmapData(bitmapSize) )
import Codec.BMP ( BMP )
import Debug.Trace ( trace )

import Types
    ( Board(pieces),
      Col(..),
      GUI(scaleFactor, hoverPosition, gap, gridLines, piecesPictures,
          boardRadius, offset),
      Position,
      World(gui, clocks, turn, board) )
import GUI.Background ( drawBackground, importBMP )


-- | Draws the game elements of the gui. 
drawGame :: World -> Picture
drawGame w = 
    pictures [b,e,h,p]
    where
        b = drawBackground w
        e = drawGameElements w
        h = drawGameHover w
        p = drawPieces w


-- Used to display all the game elements except the pieces.
drawGameElements :: World -> Picture
drawGameElements w = 
    pictures [title,timers,grid]
    where
        title = drawTitle w
        timers = drawTimers w
        grid = drawGrid w (gridLines $ gui w) []

-- Draws the black and white timers if this game element has been selected.
-- Converts the second and third element of the clocks tuple into time format (seconds to minutes and seconds).
drawTimers :: World -> Picture
drawTimers w = 
    case clocks w of
        Nothing -> blank
        Just (c1, c2, c3) -> do
            let -- create the string displaying the timer
                bstr = "black: " ++ show (c2 `div` 60 )++ ":" ++ formatSeconds (c2 `mod` 60)
                wstr = "white: " ++ show (c3 `div` 60) ++ ":" ++ formatSeconds (c3 `mod` 60)
                -- create the timer pictures
                bt = drawTimer w bstr (1.1, 0.7)
                wt = drawTimer w wstr (1.1, -0.4)
            pictures [bt, wt]

-- Formats the string representation of seconds.
formatSeconds :: Int -> String
formatSeconds sec   | sec == 0 = "00"
                    | sec < 10 = "0" ++ show sec
                    | otherwise = show sec

-- Positions and scales the text representation of the timer.
drawTimer :: World -> String -> (Float, Float) -> Picture
drawTimer w timer (x,y) =
    translate o 0 $ translate (br*x) (br*y) $ scale sf sf $ scale 0.15 0.15 $ text timer
    where
        br = boardRadius $ gui w
        sf = scaleFactor $ gui w
        o = offset $ gui w

-- Recursively creates an array of lines that are used to construct the grid using the gridLines in the gui.
-- These lines are reduced to a single picture.
drawGrid :: World -> [Float] -> [Picture] -> Picture
drawGrid w [] lns = translate (offset $ gui w) 0 $ pictures lns
drawGrid w (x:xs) lns = do  
    let br = boardRadius $ gui w
        lns' = lns ++ [
            -- add multiple lines offset to create thicker grid
            color black $ line [(x ,br), (x ,-br)],
            color black $ line [(x+1 ,br), (x+1 ,-br)],
            color black $ line [(-br, x), (br, x)],
            color black $ line [(-br, x+1), (br, x+1)]
            ]
    drawGrid w xs lns'

-- | Creates and array of floats that represent the distance of a line from the centre of the gui.
findGridLines :: Float -> Float -> Int -> Int -> [Float] -> [Float]
findGridLines _ _ _ 0 pos = pos
findGridLines br g total count pos =
    findGridLines br g total (count-1) (pos ++ [d])
        where
            d = distance br g total count

-- The distance of each line from either the far left or the top of the grid.
distance :: Float -> Float -> Int -> Int -> Float
distance br g total current = -br + g/2 + (g * fromIntegral (total-current))

-- Used to display possible moves as the player moves there mouse in the game.
drawGameHover :: World -> Picture
drawGameHover w =
    case hoverPosition $ gui w of
        Left _ -> blank
        Right (x,y) -> if x > 0 && y > 0 then drawCircle w (x,y)
                       else blank 

-- Draws an opaque circle used to represent where the player is pointing.
drawCircle :: World -> (Int, Int) -> Picture
drawCircle w (x,y) =
    translate o 0 $ translate (lns!!(x-1)) (lns!!(y-1)) $ color c $ circleSolid r
    where o = offset $ gui w
          lns = gridLines $ gui w
          c = getOpaqueColour $ turn w
          r = pieceRadius $ gap $ gui w

-- Used to create an opaque colour for a piece.
getOpaqueColour :: Col -> Color
getOpaqueColour Black = makeColor 0.152 0.149 0.149 0.5
getOpaqueColour White = makeColor 0.760 0.764 0.764 0.5

-- Calls the recursive function to create a list of pictures and reduces it to a single picture.
drawPieces :: World -> Picture
drawPieces w =
    pictures $ drawPiece w lns radius ps []
        where
            ps = pieces $ board w
            radius = pieceRadius $ gap $ gui w
            lns = gridLines $ gui w

-- Recursicvely moves through a list of pieces converting them into a list of pictures.
drawPiece :: World -> [Float] -> Float -> [(Position, Col)] -> [Picture] -> [Picture]
drawPiece _ _ _ [] pics = pics
drawPiece w lns radius (((x,y), col):ps) pics =
    drawPiece w lns radius ps (p : pics)
        where
            p = translate (offset $ gui w) 0 $ translate (lns!!(x-1)) (lns!!(y-1)) $ colPiece w col

-- Selects a piece picture based on the colour.
colPiece :: World -> Col -> Picture
colPiece w Black = fst $ piecesPictures $ gui w
colPiece w White = snd $ piecesPictures $ gui w

-- Used to calculate the radius of the piece based on the board size.
pieceRadius :: Float -> Float
pieceRadius g = g / 3

-- Scale and position the game screen title text relative the window res.
drawTitle :: World -> Picture
drawTitle w =
    translate o 0 $ translate (-br*0.9) (br*1.4) $ scale sf sf $ scale 0.4 0.4 $ text "gomoku"
    where
        br = boardRadius $ gui w
        o = offset $ gui w
        sf = scaleFactor $ gui w

-- | Creates the pictures used as pieces from imported bitmaps.
createPieces :: Float -> IO (Picture, Picture)
createPieces g = do
    (bp, wp) <- importPieces
    return (sizePiece bp Black radius, sizePiece wp White radius)
        where
            radius = pieceRadius g

-- Used to scale the piece bitmap based on the window size.
sizePiece :: Maybe BMP -> Col -> Float -> Picture
sizePiece bmp col radius =
    case getPiece bmp col radius of
        Right bmp -> do
            let data' = bitmapDataOfBMP bmp
                bsize = bitmapSize data'
                scale' = ((radius*2)/fromIntegral (fst bsize), (radius*2)/fromIntegral (fst bsize))
            uncurry scale scale' $ bitmapOfBMP bmp
        Left pic -> pic

-- Used to get a representation of a piece.
getPiece :: Maybe BMP -> Col -> Float -> Either Picture BMP
getPiece bmp Black radius =
    case bmp of
        Just b -> Right b
        _ -> Left $ color black $ circleSolid radius
getPiece bmp White radius =
    case bmp of
        Just b -> Right b
        _ -> Left $ color white $ circleSolid radius

-- Imports the bmps used to create pieces.
importPieces :: IO (Maybe BMP, Maybe BMP)
importPieces = do
    b1 <- importBMP "res/black-piece.bmp"
    b2 <- importBMP "res/white-piece.bmp"
    return (b1, b2)

