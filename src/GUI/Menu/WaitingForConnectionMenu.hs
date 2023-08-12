{-|
Module: GUI.Menu.WaitingForConnectionMenu
Description: Handles the drawing of the waiting for a connection menu

The 'drawWaitingForConnectionMenu' function is used to draw the panel used to allow a user wait for a client to connect or exit the waiting. It draws the background, the panel, and the title of the menu.

Using this menu, the user can wait to start a new game with a client.
-}
module GUI.Menu.WaitingForConnectionMenu (
    -- * Drawing the end screen
    drawWaitingForConnectionMenu,
    waitingBoundsList,
    waitingOpenBounds,
    waitingBackBounds
) where

import Graphics.Gloss

import Types
import GUI.Menu.PanelMenu
import Network.Socket
import Data.Word (Word8)
import Data.Bits
import Network.BSD (getHostName)

-- | Used to draw the waiting to connect menu.
drawWaitingForConnectionMenu :: World -> IO Picture
drawWaitingForConnectionMenu w = do
    p <- drawBackWaitingForConnectionMenu w
    return $ pictures [drawPanel w "" 0.4, p, drawWaitingForConnectionMenuHover w, drawForeWaitingForConnectionMenu w]

drawBackWaitingForConnectionMenu :: World -> IO Picture
drawBackWaitingForConnectionMenu w = do
    ip <- getIP
    return $ scale sf sf $ pictures [
        translate (-170) 40 $ color black $ scale 0.12 0.12 $ text "IP Address:",
        translate (-100) 25 $ color awhite $ rectangleSolid 130 20,
        translate (-150) 19 $ color black $ scale 0.1 0.1 $ text ip
        ]
    where
        sf = scaleFactor $ gui w
        awhite = makeColor 0.9 0.9 0.9 0.8

getIP :: IO String
getIP = do
    hostName <- getHostName
    addrinfos <- getAddrInfo Nothing (Just hostName) Nothing
    let serveraddr = head $ filter isPublicIPv4 $ map addrAddress addrinfos
    let ip = formatIp $ show serveraddr
    return ip

isPublicIPv4 :: SockAddr -> Bool
isPublicIPv4 (SockAddrInet _ hostAddr) =
    let ip =  hostAddressToString hostAddr
    in notElem ip privateIPv4Ranges
isPublicIPv4 _ = False

-- remove :0 at the end so that it is just a address like xxx.xxx.xxx.xxx
formatIp :: String -> String
formatIp = takeWhile (/= ':')


hostAddressToString :: HostAddress -> String
hostAddressToString hostAddr =
    let (a, b, c, d) = hostAddressToTuple hostAddr
    in show a ++ "." ++ show b ++ "." ++ show c ++ "." ++ show d

hostAddressToTuple :: HostAddress -> (Word8, Word8, Word8, Word8)
hostAddressToTuple hostAddr =
    let a = fromIntegral $ hostAddr `shiftR` 24
        b = fromIntegral $ hostAddr `shiftR` 16
        c = fromIntegral $ hostAddr `shiftR` 8
        d = fromIntegral hostAddr
    in (a, b, c, d)

privateIPv4Ranges :: [String]
privateIPv4Ranges =
    [ "10.0.0.0/8"
    , "172.16.0.0/12"
    , "192.168.0.0/16"
    , "169.254.0.0/16"
    , "127.0.1.1"
    ]

drawWaitingForConnectionMenuHover :: World -> Picture
drawWaitingForConnectionMenuHover w =
    case hoverPosition $ gui w of
        -- special hover effect for the play button
        Left 0 -> do
            let pic = convertPointsToRect $ head waitingBoundsList
            scale sf sf $ color pgreen pic
        -- special hover effect for the back button
        Left 1 -> do
            let pic = convertPointsToTrap $ waitingBoundsList !! 1
            scale sf sf $ color pred pic
        _ -> blank
    where
        sf = scaleFactor $ gui w
        pgreen = makeColor 0.562 0.711 0.621 0.6
        pred = makeColor 0.862 0.521 0.521 0.5

drawForeWaitingForConnectionMenu :: World -> Picture
drawForeWaitingForConnectionMenu w = scale sf sf $ pictures [
    translate (-190) (-190) $ color black $ scale 0.15 0.15 $ text "<< back",
    translate (-189) (-30) $ color black $ scale 0.15 0.15 $ text "open connection >>"
    ]
    where
        sf = scaleFactor $ gui w

waitingBoundsList :: [(Point, Point)]
waitingBoundsList = [waitingOpenBounds, waitingBackBounds]
waitingOpenBounds = ((-192, -40), (7, -9))
waitingBackBounds = ((-192, -197),(-104,-168))
