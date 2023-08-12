{-# LANGUAGE OverloadedStrings #-}
{-|
Module: Types
Description: Defines the types used in the game

The 'Col' type is used to represent the colour of a piece. The 'Position' type is used to
represent a position on the board. The 'Board' type is used to represent the state of the
game board. The 'World' type is used to represent the state of the game.

The 'other' function is used to get the other colour.

The 'ToJSON' and 'FromJSON' instances are used to encode and decode the types to and from
JSON.


-}
module Types (
  -- * Gamemode type
  GameMode(..),
  -- * Game state type
  GameState(..),
  nextGameMode,
  previousGameMode,
  -- * Game screen type
  GameScreen(..),
  -- * GUI type
  GUI(..),
  -- * Colour type
  Col(..),
  other,
  -- * Position type
  Position,
  -- * Board type
  -- $board
  Board(..),
  -- * Flag type
  Flags(..),
  -- * World type
  -- $world
  World(..),
  -- * World without gui
  WorldWithoutGUI(..)
) where

import Data.Aeson
import Network
import GHC.IO.Handle
import Graphics.Gloss
import Text.Parsec (Consumed(Empty))


-- | A gamemode references the type of opponent the player is facing.
data GameMode = Online -- ^ The player is playing against another player over the internet.
              | Local -- ^ The player is playing against another player on the same computer.
              | AI -- ^ The player is playing against an AI.
  deriving Show

nextGameMode :: GameMode -> GameMode
nextGameMode AI = Local
nextGameMode Local = Online
nextGameMode Online = AI

previousGameMode :: GameMode -> GameMode
previousGameMode AI = Online
previousGameMode Online = Local
previousGameMode Local = AI

instance Read GameMode where
  readsPrec _ value =
    case value of
      "Online" -> [(Online, "")]
      "Local" -> [(Local, "")]
      "AI" -> [(AI, "")]
      _ -> []

instance Eq GameMode where
    Online == Online = True
    Local == Local = True
    AI == AI = True
    _ == _ = False

instance FromJSON GameMode where
  parseJSON (String "Online") = return Online
  parseJSON (String "Local") = return Local
  parseJSON (String "AI") = return AI
  parseJSON _ = fail "Invalid GameMode"

instance ToJSON GameMode where
  toJSON Online = "Online"
  toJSON Local = "Local"
  toJSON AI = "AI"


-- | A game state is used to track where the player is in the application.
data GameState = InPlay -- ^ The player is currently playing a game.
               | Draw -- ^ The game has ended in a draw.
               | Win Col -- ^ The game has ended with a winner.
               | Menu -- ^ The player is currently in a menu.
               | Playback [WorldWithoutGUI] Int -- ^ The player is currently watching a playback of a game.

instance Eq GameState where
    InPlay == InPlay = True
    Draw == Draw = True
    Win Black == Win Black = True
    Win White == Win White = True
    Menu == Menu = True
    Playback _ _ == Playback _ _ = True
    _ == _ = False

instance Show GameState where
  show InPlay = "InPlay"
  show Draw = "Draw"
  show (Win Black) = "Win Black"
  show (Win White) = "Win White"
  show Menu = "Menu"

instance FromJSON GameState where
  parseJSON (String "InPlay") = return InPlay
  parseJSON (String "Draw") = return Draw
  parseJSON (String "Menu") = return Menu
  parseJSON (String "Win Black") = return (Win Black)
  parseJSON (String "Win White") = return (Win White)
  parseJSON _ = fail "Invalid GameState"

instance ToJSON GameState where
  toJSON InPlay = "InPlay"
  toJSON Draw = "Draw"
  toJSON Menu = "Menu"
  toJSON (Win Black) = "Win Black"
  toJSON (Win White) = "Win White"


-- | A game screen tracks what the user is currently interacting with.
data GameScreen = Main -- ^ The main menu.
                | Options -- ^ The options menu.
                | GameBoard -- ^ The game board.
                | Pause -- ^ The pause menu.
                | Connect -- ^ The connect menu.
                | WaitingForConnection -- ^ The waiting for connection menu.

instance Eq GameScreen where
    Main == Main = True
    Options == Options = True
    GameBoard == GameBoard = True
    Pause == Pause = True
    Connect == Connect = True
    WaitingForConnection == WaitingForConnection = True
    _ == _ = False

data Flags = Empty
           | Concede
           | Time


-- | Holds all the images and the calculated data relating to the gui. This prevents the need to re-import and constantly calculate the same information.
data GUI = GUI { windowResolution :: (Int, Int), -- ^ The resolution of the window.
                 scaleFactor :: Float, -- ^ The scale factor of the window.
                 currentScreen :: GameScreen, -- ^ The current screen the user is interacting with.
                 boardRadius :: Float, -- ^ The radius of the board.
                 gap :: Float, -- ^ The gap between the board and the edge of the window.
                 offset :: Float, -- ^ The offset of the board from the centre of the window.
                 gridLines :: [Float], -- ^ The positions of the grid lines.
                 hoverPosition :: Either Int Position, -- ^ The position of the mouse cursor.
                 piecesPictures :: (Picture, Picture), -- ^ The pictures of the pieces.
                 background :: Picture -- ^ The background picture.
                 }


-- | Define the colour of a piece
data Col = Black
         | White
  deriving Show

instance Eq Col where
  Black == Black = True
  White == White = True
  _ == _ = False

instance Read Col where
  readsPrec _ value =
    case value of
      "Black" -> [(Black, "")]
      "White" -> [(White, "")]
      _ -> []

instance ToJSON Col where
  toJSON Black = "Black"
  toJSON White = "White"

instance FromJSON Col where
  parseJSON (String "Black") = return Black
  parseJSON (String "White") = return White
  parseJSON _ = fail "Invalid Col"

-- | Get the other colour
other :: Col -- ^ The colour to get the other of
      -> Col -- ^ The other colour
other Black = White
other White = Black

-- | Define a position on the board
type Position = (Int, Int)



-- }}}
-- {{{ Board type

-- $board
-- A Board is a record containing the board size (a board is a square grid,
-- n * n), the number of pieces in a row required to win, and a list 
-- of pairs of position and the colour at that position.  So a 10x10 board 
-- for a game of 5 in a row with a black piece at 5,5 and a white piece at 8,7
-- would be represented as:
--
-- Board 10 5 [((5, 5), Black), ((8,7), White)]

-- | Define the state of the game board
data Board = Board { size :: Int, -- ^ The size of the board.
                     target :: Int, -- ^ The number of pieces in a row required to win.
                     pieces :: [(Position, Col)] -- ^ The positions of the pieces on the board.
                   }
  deriving Show


instance Eq Board where
  (Board s1 t1 p1) == (Board s2 t2 p2) = s1 == s2 && t1 == t2 && p1 == p2

instance ToJSON Board where
  toJSON (Board s t p) = object ["size" .= s, "target" .= t, "pieces" .= p]

instance FromJSON Board where
  parseJSON (Object v) = Board <$> v .: "size" <*> v .: "target" <*> v .: "pieces"
  parseJSON _ = fail "Invalid Board"


-- }}}
-- {{{ World type

-- $world
-- Overall state is the board, the colour of the current player, the game mode,
-- if the current player is waiting for a connection or is connecting, the host, 
-- the previous boards, whether the pause menu is shown, and the clocks.

-- | Define the overall state of the game
data World = World { state :: GameState, -- ^ The current state of the game.
                     board :: Board, -- ^ The current state of the board.
                     gameMode :: GameMode, -- ^ The current game mode.
                     turn :: Col, -- ^ The current player's colour.
                     winner :: Maybe Col, -- ^ The winner of the game.
                     local_online :: String, -- ^ Whether the player is waiting for a connection or is connecting.
                     host :: HostName, -- ^ The host name to connect to.
                     previousBoards :: [Board], -- ^ The previous boards.
                     clocks :: Maybe (Int, Int, Int), -- ^ The clocks.]
                     socket :: Maybe Socket, -- ^ The socket
                     handle :: Maybe Handle, -- ^ The handle
                     flags :: Flags, -- ^ The flags for the world.
                     gui :: GUI -- ^ The gui.
                     }

instance Show World where
  show (World s b gm c wn lo h pbs clks so ha f g) = show b ++ show gm ++ show c ++ show pbs ++ show clks

-- | Define the overall state of the game without the gui. This is needed as the gui should not be sent over the network and should stay local to each player.
data WorldWithoutGUI = WorldWithoutGUI { state' :: GameState, -- ^ The current state of the game.
                                         board' :: Board, -- ^ The current state of the board.
                                         gameMode' :: GameMode, -- ^ The current game mode.
                                         turn' :: Col, -- ^ The current player's colour.
                                         winner' :: Maybe Col, -- ^ The winner of the game.
                                         local_online' :: String, -- ^ Whether the player is waiting for a connection or is connecting.
                                         host' :: HostName, -- ^ The host name to connect to.
                                         previousBoards' :: [Board], -- ^ The previous boards.
                                         clocks' :: Maybe (Int, Int, Int) -- ^ The clocks.
                                         }

instance Show WorldWithoutGUI where
  show (WorldWithoutGUI s b gm c wn lo h pbs clks) = show b ++ show gm ++ show c ++ show pbs ++ show clks

instance Eq WorldWithoutGUI where
  (WorldWithoutGUI s1 b1 gm1 c1 wn1 lo1 h1 pbs1 clks1) == (WorldWithoutGUI s2 b2 gm2 c2 wn2 lo2 h2 pbs2 clks2) = s1 == s2 && b1 == b2 && gm1 == gm2 && c1 == c2 && lo1 == lo2 && h1 == h2 && pbs1 == pbs2 && clks1 == clks2

instance FromJSON WorldWithoutGUI where
  parseJSON (Object v) = WorldWithoutGUI <$> v .: "state" <*> v .: "board" <*> v .: "gameMode" <*> v .: "turn" <*> v .: "winner" <*> v .: "local_online" <*> v .: "host" <*> v .: "previousBoards" <*> v .: "clocks"
  parseJSON _ = fail "Invalid WorldWithoutGUI"

instance ToJSON WorldWithoutGUI where
  toJSON (WorldWithoutGUI s b gm c wn lo h pbs clks) = object ["state" .= s, "board" .= b, "gameMode" .= gm, "turn" .= c, "winner" .= wn, "local_online" .= lo, "host" .= h, "previousBoards" .= pbs, "clocks" .= clks]