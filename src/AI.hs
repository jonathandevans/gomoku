{-|
Module: AI
Description: Defines the AI and the `updateWorld` function

The 'buildTree' function is used to generate a game tree from a board state and a move generator.

The 'updateWorld' function is used to update the world state 10 times per second.
-}
module AI where

import Board
import Data.Bifunctor (Bifunctor(bimap))
import Data.List (nub)
import Types
import qualified Data.Maybe
import System.IO (hReady, hClose)
import Debug.Trace (trace)
import Protocol
import Data.Typeable
import Network.Socket (Family(AF_802), sClose)
import Data.Maybe (fromJust)
import qualified Data.Foldable



data GameTree = GameTree { game_board :: Board,
                           game_turn :: Col,
                           next_moves :: [(Position, GameTree)] }

-- | Given a function to generate plausible moves (i.e. board positions)
-- for a player (Col) on a particular board, generate a (potentially)
-- infinite game tree.
--
-- (It's not actually infinite since the board is finite, but it's sufficiently
-- big that you might as well consider it infinite!)
--
-- An important part of the AI is the 'gen' function you pass in here.
-- Rather than generating every possible move (which would result in an
-- unmanageably large game tree!) it could, for example, generate moves
-- according to various simpler strategies.
buildTree :: (Board -> Col -> [Position]) -- ^ Move generator
             -> Board -- ^ board state
             -> Col -- ^ player to play next
             -> GameTree
buildTree gen b c = let moves = gen b c in -- generated moves
                        GameTree b c (mkNextStates moves)
  where
    mkNextStates :: [Position] -> [(Position, GameTree)]
    mkNextStates [] = []
    mkNextStates (pos : xs)
        = case makeMove b c pos of -- try making the suggested move
               Nothing -> mkNextStates xs -- not successful, no new state
               Just b' -> (pos, buildTree gen b' (other c)) : mkNextStates xs
                             -- successful, make move and build tree from 
                             -- here for opposite player

-- Get the best next move from a (possibly infinite) game tree. This should
-- traverse the game tree up to a certain depth, and pick the move which
-- leads to the position with the best score for the player whose turn it
-- is at the top of the game tree.
getBestMove :: Int -- ^ Maximum search depth
               -> GameTree -- ^ Initial game tree
               -> Position
getBestMove depth tree = undefined


-- | Update the world state after some time has passed. Calls the 'incrementClock'and 'requestNetwork' functions.
updateWorld :: Float -- ^ time since last update (you can ignore this)
            -> World -- ^ current world state
            -> IO World
updateWorld t w =  case state w of
        Playback ws c -> case currentScreen (gui w) of
            GameBoard -> playback w ws c
            _ -> return w
        Win _ -> if gameMode w == Online then do
            Data.Foldable.forM_ (socket w) sClose
            hClose (fromJust $ handle w) >> return w
                    else return w
        InPlay -> case gameMode w of
            Online -> do
                w' <- requestNetwork w
                incrementClock w'
            _ -> do
                incrementClock w
        _ -> return w



playback :: World -> [WorldWithoutGUI] -> Int -> IO World
playback w [] _ = return w
playback w [x] c =
    case c of
        5 -> do
            let w' = w { board = (board' x), gui = (gui w) {currentScreen = GameBoard}, clocks = clocks' x}
            return w' {state = (state' x)}
        _ -> return w {state = Playback [x] (c+1)}
playback w (x:xs) c =
    case c of
        5 -> do
            let w' =  w { board = (board' x), gui = (gui w) {currentScreen = GameBoard}, clocks = clocks' x}
            return w' {state = Playback xs 0}
        _ -> return w {state = Playback (x:xs) (c+1)}


-- | Increment the clock tuple by 1. When it reaches 10, if it is white's turn then decrement the 
-- second element of the clock tuple by 1. If it is black's turn then decrement the third element 
-- of the clock tuple by 1. If either of the clock elements reach 0, then the game is over and 
-- the player who's clock reached 0 loses.
incrementClock :: World -- ^ current world state
               -> IO World -- ^ updated world state
incrementClock w
    | gameMode w == Online && state w == InPlay = incrementClock' w
    | currentScreen (gui w)== GameBoard && state w == InPlay = incrementClock' w
    | otherwise = return w

incrementClock' :: World -- ^ current world state
               -> IO World -- ^ updated world state

incrementClock' w = do
    case clocks w of
            Just (c1, c2, c3) -> do
                if c1 == 10 then
                    if turn w == White then
                        if c3 == 0 then
                            return w {clocks = Just (0, 0, 0)}
                        else
                            return w {clocks = Just (0, c2, c3 -1)}
                    else
                        if c2 == 0 then
                            return w {clocks = Just (0, 0, 0)}
                        else
                            return w {clocks = Just (0, c2 - 1, c3)}
                else
                    return w {clocks = Just (c1 + 1, c2, c3)}
            Nothing -> return w

-- | Read message from socket if possible and update world state.
requestNetwork :: World -- ^ current world state
               -> IO World -- ^ updated world state
requestNetwork w = do
    ready <- hReady (Data.Maybe.fromJust (handle w))
    if ready then do
        trace "about to read message" $ return ()
        msg <- readMessage (handle w)
        trace "message read" $ return ()
        let w' = worldWithoutGUIToWorldWithHandles (byteStringToWorldWithoutGUI msg) (gui w) (socket w) (handle w)
        trace ("created world: " ++ show w') $ return ()
        return w'
    else return w

{- Hint: 'updateWorld' is where the AI gets called. If the world state
 indicates that it is a computer player's turn, updateWorld should use
 'getBestMove' to find where the computer player should play, and update
 the board in the world state with that move.

 At first, it is reasonable for this to be a random move!

 If both players are human players, the simple version above will suffice,
 since it does nothing.

 In a complete implementation, 'updateWorld' should also check if either 
 player has won and display a message if so.
-}


-- checks what moves are viable for the ai to make, if the board is empty, return the middle of the board. 
checkViableMoves :: Board -> Col -> [Position]

checkViableMoves (Board n _ []) _ = [(n`div`2,n`div`2)]

checkViableMoves (Board n _ xs) _ = nub (filter (`notElem` locations) (getNexts locations []))
                                     where
                                        locations = map fst xs
--recursively gets the neighbours of each of the played pieces on the board
getNexts :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
getNexts (x:xs) rs = getNexts xs adder
                          where adder = map (\(p, y) -> bimap (+ p) (+ y) x) dirs
                                dirs = [(1, 0), (0, 1), (1, 1), (1, -1), (-1, 0), (0, -1), (-1, -1), (-1, 1)]
getNexts _ rs = rs