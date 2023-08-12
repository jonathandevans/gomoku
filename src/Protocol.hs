{-# LANGUAGE OverloadedStrings #-}

{-|
Module: Protocol
Description: Defines the message format for the client-server communication protocol

This module defines the message format for the client-server communication protocol. It also defines functions to create handles to the server and client.

The 'writeMessage' and 'readMessage' functions are used to write and read messages to and from the server. 

The 'createServerHandle' and 'createClientHandle' functions are used to create handles for the server and client respectively.

The 'worldWithoutGUIToByteString' and 'byteStringToWorldWithoutGUI' functions are used to convert a 'WorldWithoutGUI' to a 'ByteString' and vice versa.

The 'worldToWorldWithoutGUI' and 'worldWithoutGUIToWorld' functions are used to convert a 'World' to a 'WorldWithoutGUI' and vice versa.

The 'worldWithoutGUIToWorldWithHandles' function is used to convert a 'WorldWithoutGUI' to a 'World' with a handle.
-}

module Protocol (
  -- * Reading and writing with sockets
  writeMessage,
  readMessage,
  -- * Create handles
  createServerHandle,
  createClientHandle,
  -- * Conversion tools
  worldWithoutGUIToByteString,
  byteStringToWorldWithoutGUI,
  worldToWorldWithoutGUI,
  worldWithoutGUIToWorld,
  worldWithoutGUIToWorldWithHandles
) where

import Data.Aeson ( eitherDecode, encode )
import Data.Aeson.Types (typeMismatch)
import System.IO (hSetBuffering, hGetLine, hPutStrLn, BufferMode(..), IOMode (WriteMode, ReadMode), hReady)
import Network.Socket
    ( Socket, setSocketOption, SocketOption(ReuseAddr) ) 
import Network
    ( accept, connectTo, listenOn, PortID(PortNumber), Socket ) 

import Types
import qualified Data.Maybe
import Data.Maybe (isJust)
import Data.Functor
import GHC.IO.Handle ( Handle, hFlush )
import Debug.Trace (trace)
import qualified Data.ByteString.Char8 as BS
import Data.ByteString 

-- | Write to the socket
writeMessage :: Maybe Handle -- ^ The socket/handle to write to
             -> ByteString -- ^ The message to write
             -> IO () -- ^ The result of the write
writeMessage h msg = do
  BS.hPutStrLn (Data.Maybe.fromJust h) msg
  hFlush (Data.Maybe.fromJust h)

-- | Read from the socket
readMessage :: Maybe Handle -- ^ The socket/handle to read from
            -> IO ByteString -- ^ The message read
readMessage h = do
  msg <- BS.hGetLine (Data.Maybe.fromJust h)
  hFlush (Data.Maybe.fromJust h)
  return msg

-- create server handle
createServerHandle :: IO (Socket, Handle) -- ^ The handle to the server
createServerHandle = do
  trace "Create server socket" $ return ()
  sock <- listenOn (PortNumber 23102)
  trace "Server socket created" $ return ()
  setSocketOption sock ReuseAddr 1
  trace "Attempting to accept connection" $ return ()
  (handle, clientHost, port) <- Network.accept sock
  trace ("Connection accepted from " ++ clientHost) $ return ()
  return (sock, handle)

-- create client handle
createClientHandle :: String -- ^ The host to connect to
                   -> IO Handle -- ^ The handle to the server
createClientHandle host = do
  trace ("Attempting to connect to " ++ host) $ return ()
  connectTo host (PortNumber 23102)

-- | Convert a World to a ByteString
worldWithoutGUIToByteString :: WorldWithoutGUI -- ^ The WorldWithoutGUI to convert
                            -> ByteString -- ^ The converted ByteString
worldWithoutGUIToByteString = toStrict . encode

-- | Convert a ByteString to a World
byteStringToWorldWithoutGUI :: ByteString -- ^ The ByteString to convert
                            -> WorldWithoutGUI -- ^ The converted WorldWithoutGUI
byteStringToWorldWithoutGUI = either error id . eitherDecode . fromStrict

-- | Convert a World to a WorldWithoutGUI
worldToWorldWithoutGUI :: World -- ^ The World to convert
                       -> WorldWithoutGUI -- ^ The converted WorldWithoutGUI
worldToWorldWithoutGUI w = WorldWithoutGUI (state w) (board w) (gameMode w) (turn w) (winner w) (local_online w) (host w) (previousBoards w) (clocks w)

-- | Convert a WorldWithoutGUI to a World
worldWithoutGUIToWorld :: WorldWithoutGUI -- ^ The WorldWithoutGUI to convert
                       -> GUI -- ^ The GUI to use
                       -> World -- ^ The converted World
worldWithoutGUIToWorld w = World (state' w) (board' w) (gameMode' w) (turn' w) (winner' w) (local_online' w) (host' w) (previousBoards' w) (clocks' w) Nothing Nothing Empty

worldWithoutGUIToWorldWithHandles :: WorldWithoutGUI -- ^ The WorldWithoutGUI to convert
                                  -> GUI -- ^ The GUI to use
                                  -> Maybe Socket -- ^ The socket to the server
                                  -> Maybe Handle -- ^ The handle to the server
                                  -> World -- ^ The converted World
worldWithoutGUIToWorldWithHandles w gui so ha = World (state' w) (board' w) (gameMode' w) (turn' w) (winner' w) (local_online' w) (host' w) (previousBoards' w) (clocks' w) so ha Empty gui