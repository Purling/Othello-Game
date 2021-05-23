{-|
Module      : AI
Description : AIs for Othello
Copyright   : (c) 2020 Your Name Here
License     : AllRightsReserved
-}
module AI where

import Othello

-- | Type of AI functions you can choose to write.
data AIFunc
  = NoLookahead (GameState -> Move)
    -- ^ Simple AIs that do not need lookahead.
  | WithLookahead (GameState -> Int -> Move)
    -- ^ AIs that want to look ahead. The assignment framework will
    -- call the function over and over with increasing integer
    -- arguments @1, 2, 3, ...@ until your AI's time limit is up.

-- | The table of all AIs that your assignment provides. The AI named
-- "default" in this table is the one your tutor will dedicate most of
-- his or her attention to marking.
ais :: [(String, AIFunc)]
ais = [ ("firstLegalMove", NoLookahead firstLegalMove),
        ("greedyStrategy", NoLookahead greedyAI)
      ]

-- | A very simple AI, which picks the first move returned by the
-- 'legalMoves' function. AIs can rely on the 'legalMoves' list being
-- non-empty; if there were no legal moves, the framework would have
-- ended the game.
firstLegalMove :: GameState -> Move
firstLegalMove st = head (legalMoves st)

-- | A greedy AI with greediness based completely on the most pieces that can
-- be captured on the next available move.
greedyAI :: GameState -> Move
greedyAI (GameState bounds turn board) = case turn of
  GameOver _ -> error "greedyAI: Invalid Input (Gameover is invalid)"
  _          -> greedyHeuristic (legalMoves (GameState bounds turn board))
                 (GameState bounds turn board)


-- | A function which uses a heuristic to determine the next best move. The
-- next best move is decided entirely on immediate score improvement.
greedyHeuristic :: [Move] -> GameState -> Move
greedyHeuristic [] _ = Move (9,9)
greedyHeuristic (x:xs) (GameState bounds turn board) = case x:xs of
  [] -> x
  _  -> case turn of
    GameOver _  -> error "greedyAI: Invalid Input (Gameover is invalid)"
    Turn _ -> snd (foldr1 maxTuple (map score tupleList))
  where
    gameStateList :: [Maybe GameState]
    gameStateList = map (applyMove (GameState bounds turn board)) (x:xs)
    tupleList :: [(Maybe GameState, Move)]
    tupleList = zip gameStateList (x:xs)


compare :: Board -> Player -> Int
compare board player = max (currentScore board player) 0

-- (map ((applyMove (GameState bounds turn board)) (x:xs))) 
-- Gives out a list of Maybe GameState

-- map score (map ((applyMove (GameState bounds turn board)) (x:xs)))
-- Returns a list of scores

-- Gives out a tuple of lists given a list and another list
-- zip (map (applyMove (initialState (8,8))) li) li

-- Turns a Maybe GameState list into a list of ints
-- score :: Maybe GameState -> Int
-- score gameState = case gameState of
--   (Just (GameState _ turn board)) -> 
--     case turn of
--       GameOver _  -> error "greedyAI: Invalid Input (Gameover is invalid)"
--       Turn player -> currentScore board player
--   _ -> -1

-- Turns a tuple Maybe GameState list into a list of ints and moves
score :: (Maybe GameState, Move) -> (Int, Move)
score (gameState,move) = case gameState of
  (Just (GameState _ turn board)) ->
    case turn of
      GameOver _  -> (0,move)
      Turn player -> (currentScore board player, move)
  _ -> (-1,move)

-- A max function for tuples. Can be polymorphic
maxTuple :: (Int, Move) -> (Int, Move) -> (Int, Move)
maxTuple (a,b) (c,d)
  | a > c = (a,b)
  | otherwise = (c,d)

li :: [Move]
li = [Move (3,2), Move (2,3), Move (3,3)]

-- Testing function: foldr max (0) (map score (map (applyMove (initialState (8,8))) li))
-- Gives out the max int from a list of ints
-- (map score' zip (map (applyMove (initialState (8,8))) li) li))
-- Gives out a list of (int, move)
-- foldr max' (0,Move (1,1)) (map score' zip (map (applyMove (initialState (8,8))) li) li))
-- Gives out the tuple which has the most score advantage (Score, Move)