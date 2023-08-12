{-|
Module: Board
Description: Defines the initial state of the World, the functions used to make moves and save/load games, and the function used to check if a game has been won.

The 'initWorld' function is used to initialise a world using arguments.

The 'makeMove' function is used to make a move on the board.

The 'undoMove' function is used to undo a move on the board.

The 'saveGame' function is used to save a game.

The 'loadGame' function is used to load a game.

The 'makeWon' function is used to check if a game has been won.
-}
module Board (
  -- * Initialisation
  initWorld,
  initWorldOnline,
  -- * Moves
  makeMove,
  undoMove,
  -- * Saving and loading
  saveGame,
  loadGame,
  -- * Checking for win
  makeWon
) where

import Parser.WorldParser
import Types
import Protocol
import Text.Parsec
import Network.Socket ( HostName, Socket )
import GHC.IO.Handle (Handle)
import GUI.Draw
import GUI.GameScreen

import qualified Data.Bifunctor
import Data.Maybe (mapMaybe)
import Data.List (find)
import Debug.Trace (trace)

-- Initialise a board using arguments
initBoard :: Int -- ^ Size of the board
          -> Int -- ^ Target number of pieces in a row
          -> Board -- ^ Initialised board
initBoard size goal = Board size goal []


-- | Initialise a world using arguments
initWorld :: (GameState, Int, Int, GameMode, Col, HostName, Maybe (Int,Int,Int)) -- ^ Arguments
          -> GUI -- ^ GUI
          -> IO World -- ^ Initialised world
initWorld (st, s, tar, gm, c, hn, cl) g =
    case gm of
        Online ->
            case c of
                White -> do
                    (socket, handle) <- createServerHandle
                    initWorldOnline (st, s, tar, gm, c, hn, cl) g (Just socket) handle
                Black -> do
                    handle <- createClientHandle hn
                    initWorldOnline (st, s, tar, gm, c, hn, cl) g Nothing handle
        _ -> initLocalWorld (st, s, tar, gm, c, hn, cl) g

-- | Initialise a local world using arguments
initLocalWorld :: (GameState, Int, Int, GameMode, Col, HostName, Maybe (Int,Int,Int)) -- ^ Arguments
          -> GUI -- ^ GUI
          -> IO World -- ^ Initialised world
initLocalWorld (st, s, tar, gm, c, hn, cl) g = do
    pcs <- createPieces gp
    let g' = g { currentScreen = if st == Menu then Main else GameBoard,
                 gap = gp,
                 gridLines = findGridLines br gp s s [],
                 piecesPictures = pcs
                 }
    return $ World st (initBoard s tar) gm c Nothing "Local" hn [] cl Nothing Nothing Types.Empty g'
    where
        br = boardRadius g
        gp = getGap br s


-- | Initialise a world like above but with two handles
initWorldOnline :: (GameState, Int, Int, GameMode, Col, HostName, Maybe (Int,Int,Int)) -- ^ Arguments
                -> GUI -- ^ GUI
                -> Maybe Socket -- ^ Socket
                -> Handle -- ^ Server handle
                -> IO World -- ^ Initialised world
initWorldOnline (st, s, tar, gm, c, hn, cl) g socket handle = do
    pcs <- createPieces gp
    let g' = g { currentScreen = if st == Menu then Main else GameBoard,
                 gap = gp,
                 gridLines = findGridLines br gp s s [],
                 piecesPictures = pcs
                 }
    if c == White
        then do
            -- send server settings to client
            writeMessage (Just handle) (worldWithoutGUIToByteString (worldToWorldWithoutGUI (World st (initBoard s tar) gm (other c) Nothing "Local" hn [] cl socket (Just handle) Types.Empty g')))
            return $ World st (initBoard s tar) gm (other c) Nothing "Remote" hn [] cl socket (Just handle) Types.Empty g'
    else do
            -- receive server settings from server
            w <- readMessage (Just handle)
            return $ worldWithoutGUIToWorldWithHandles (byteStringToWorldWithoutGUI w) g' socket (Just handle)
    where
        br = boardRadius g
        gp = getGap br s


-- | Play a move on the board; return 'Nothing' if the move is invalid
-- (e.g. outside the range of the board, or there is a piece already there)
makeMove :: Board -- ^ Board
         -> Col -- ^ Colour of the piece to be placed
         -> Position -- ^ Position of the piece to be placed
         -> Maybe Board -- ^ Board with the move made
makeMove (Board sz tgt pcs) col pos@(x, y)
    | x < 1 || x > sz || y < 1 || y > sz = Nothing -- outside range of board
    | pos `elem` map fst pcs = Nothing -- piece already there
    | otherwise = Just $ Board sz tgt ((pos, col) : pcs)

-- | Undo the last move using the previous board
undoMove :: World -- ^ World
         -> World -- ^ World with the last move undone
undoMove (World state board gamemode col winner local_online host (prevBoard1 : prevBoards) clocks socket handle f g) =
    World state prevBoard1 gamemode (other col) winner local_online host prevBoards clocks socket handle f g

-- | Save game
saveGame :: World -- ^ World to be saved 
         -> IO () -- ^ IO action to save the game
saveGame w = writeFile "save.txt" (show w)

-- | Load game using parsec
loadGame :: World -- ^ World
         -> IO World -- ^ IO action to load the game
loadGame w = do
    -- add exception handling for file that doesn't exist
    contents <- readFile "save.txt"
    case parse worldParser "save.txt" contents of
        Left err -> return w {state=Menu}
        Right x -> return $ worldWithoutGUIToWorld x $ gui w



-- | Check whether the board is in a winning state for either player. Only check around the position of the last move.
-- Returns 'Nothing' if neither player has won yet
-- Returns 'Just c' if the player 'c' has won
makeWon :: Board -- ^ Board state
        -> Maybe Col -- ^ Winning colour
makeWon b = if checkPoint b (snd (head (pieces b))) (fst (head (pieces b))) >= 1000000 then Just (snd (head (pieces b))) else Nothing



{- Hint: One way to implement 'checkWon' would be to write functions 
which specifically check for lines in all 8 possible directions
(NW, N, NE, E, W, SE, SW)

In these functions:
To check for a line of n in a row in a direction D:
For every position ((x, y), col) in the 'pieces' list:
- if n == 1, the colour 'col' has won
- if n > 1, move one step in direction D, and check for a line of
  n-1 in a row.
-}

-- An evaluation function for a minimax search. Given a board and a colour
-- return an integer indicating how good the board is for that colour.

-- Evaluate the given board state for the given player, returning a score
-- that measures how favorable the board state is for that player.
-- Higher scores are better for the given player, lower scores are better
-- for the opponent.

-- Possibly bugged, this might just look for winning lines even after makeWon 
-- has been called. Needs testing and discussion on what a good evaluation 
-- function should look like for a minimax search.
evaluate :: Board -> Col -> Int
evaluate b@(Board sz tgt pcs) color =
  let pieces = filter (\(_, c) -> c == color) pcs
      pieces' = filter (\(_, c) -> c /= color) pcs
      positions = map fst pieces
      positions' = map fst pieces'
      values = map (checkPoint b color) positions
      values' = map (checkPoint b (other color)) positions'
      in sum values-sum values'



-- evaluates the board for a piece played at a position and returns the score
checkPoint :: Board -> Col -> Position -> Int
checkPoint b@(Board sz tgt pcs) color (x,y) =
                        trace ("checkPoint: " ++ show (x,y) ++ " " ++ show (sum (map (getValue tgt) values))) $
                        sum (map (getValue tgt) values)
                         where
                            values = map (checkDirection b (x,y) color) directions
                            directions = [(1,0), (1,1), (0,1), (-1,1)]



-- check length of pieces in one direction and whether they are live or dead
checkDirection :: Board -> Position -> Col -> (Int, Int) -> (Int, Bool)
checkDirection b@(Board sz tgt pcs) (x,y) color (dx, dy) =
  let
    isWithinBoard pos@(px,py) = px >= 1 && px <= sz && py >= 1 && py <= sz
    getNextPos (px,py) (px',py') = (px+px', py+py')
    isDifferentColor (_, c) = c /= color
    getPiece pos = case filter (\(p,_) -> p == pos) pcs of
                     (x:_) -> x
                     [] -> ((-1,-1), color)
    go pos dir@(dx,dy) acc
      | getPiece pos == ((-1,-1), color) = (acc, False)
      | not (isWithinBoard pos) || isDifferentColor (getPiece pos) = (acc, True)
      | otherwise = go (getNextPos pos dir) dir (acc+1)
  in
    let (count1, ended1) = go (x+dx, y+dy) (dx,dy) 0
        (count2, ended2) = go (x-dx, y-dy) (-dx,-dy) 0
    in
      (count1 + count2, ended1 && ended2)


-- returns the value of a line of pieces
getValue :: Int -> (Int, Bool) -> Int
getValue target (count, ended)
        | target-count == 1 = 1000000
        | target-count == 2 && not ended = 100000
        | target-count == 3 && not ended = 10000 
        | target-count == 4 && not ended = 1000
        | target-count == 2 &&  ended = 10000
        | target-count == 3 &&  ended = 1000
        | target-count == 4 &&  ended = 100
        | otherwise = 0
