{-|
Module: GUI.Background
Description: Handles the background of the game, creating and drawing it.

The 'drawBackground' function is used draw the background based on the picture stored in the gui element of the world state.

The 'createBackground' function is used to create the picture that will be used as the background on startup. This pictured is made up of imported bitmaps.
By storing the background as a picture, it can be drawn quickly and easily and we prevent repeated loading of the bitmaps.

The 'importBMP' function is used to import a bitmap from a file.
-}
module GUI.Background (
    -- * Drawing the background
    drawBackground,
    -- * Creating the background
    createBackground,
    -- * Importing bitmaps
    importBMP
) where

import Graphics.Gloss
    ( blank,
      color,
      pictures,
      rectangleSolid,
      rectangleWire,
      scale,
      translate,
      makeColor,
      bitmapDataOfBMP,
      bitmapOfBMP,
      Picture,
      BitmapData(bitmapSize) )
import Codec.BMP ( BMP, readBMP )

import Types ( World(gui), Col(..), GUI(background) )


-- | Draws the background picture stored in the world state.
drawBackground :: World -> Picture
drawBackground w = background $ gui w

-- | Creates a picture containing the background.
createBackground :: Float -> Float -> Float -> IO Picture
createBackground br sf o = do
    (bak, bpot, wpot) <- importBackgroundImages
    let bak' = createBoardBackground bak br o
        pots = createPots (bpot, wpot) br o
    return $ pictures [bak', pots]

-- Create and position a background board for the grid.
createBoardBackground :: Maybe BMP -> Float -> Float -> Picture
createBoardBackground bmp br o =
    translate o 0 $ pictures [
        sizeBoardBackground bmp length,
        -- creates a border around the board
        color border $ rectangleWire length length,
        color border $ rectangleWire (length+1) (length+1),
        color border $ rectangleWire (length+2) (length+2),
        color border $ rectangleWire (length+3) (length+3)
        ]
    where border = makeColor 0.180 0.164 0.145 0.9
          length = br*2*1.05

-- Scale the background bitmap based on the widow size.
sizeBoardBackground :: Maybe BMP -> Float -> Picture
sizeBoardBackground bmp length =
    case getBoardBackground bmp length of
        Right bmp' -> scaleBitmap bmp' length length
        Left pic -> pic

-- Get the bmp of the background held in the world state.
getBoardBackground :: Maybe BMP -> Float -> Either Picture BMP
getBoardBackground bmp length =
    case bmp of
        Just b -> Right b
        _ -> do Left $ color (makeColor 0.435 0.333 0.227 1) $ rectangleSolid length length

-- Create a picture representing the bmp pots.
createPots :: (Maybe BMP, Maybe BMP) -> Float -> Float -> Picture
createPots (bp, wp) br o =
    pictures [bp', wp']
    where
        bp' = positionPot bp br (1.5,0.4) o Black
        wp' = positionPot wp br (1.5,-0.7) o White

-- Position the pot based on the colour relative to the board.
positionPot :: Maybe BMP -> Float -> (Float, Float) -> Float -> Col -> Picture
positionPot bmp br (x,y) o col =
    translate o 0 $ translate (br*x) (br*y) $ sizePot bmp br col

-- Scale the pot bmps based on the window size.
sizePot :: Maybe BMP -> Float -> Col -> Picture
sizePot bmp br col =
    case getPot bmp col of
        Right bmp -> do
            let length = br / 2
            scaleBitmap bmp length length
        Left pic -> pic

-- Get the bmp of the black and white pots in the world state.
getPot :: Maybe BMP -> Col -> Either Picture BMP
getPot bmp Black =
    case bmp of
        Just b -> Right b
        _ -> Left blank
getPot bmp White =
    case bmp of
        Just b -> Right b
        _ -> Left blank

-- Scale a bmp based on the given x and y values.
scaleBitmap :: BMP -> Float -> Float -> Picture
scaleBitmap bmp x y =
    let data' = bitmapDataOfBMP bmp
        bsize = bitmapSize data'
        scale' = (x/fromIntegral (fst bsize), y/fromIntegral (snd bsize))
    in uncurry scale scale' $ bitmapOfBMP bmp

-- Imports the bmps used to create the background.
importBackgroundImages :: IO (Maybe BMP, Maybe BMP, Maybe BMP)
importBackgroundImages = do
    b1 <- importBMP "res/board-background.bmp"
    b2 <- importBMP "res/black-pot.bmp"
    b3 <- importBMP "res/white-pot.bmp"
    return (b1, b2, b3)

-- | Imports a bmp from the given file path.
importBMP :: FilePath -> IO (Maybe BMP)
importBMP fp = do
    b <- readBMP fp
    case b of
        Right b' -> return $ Just b'
        Left _ -> return Nothing