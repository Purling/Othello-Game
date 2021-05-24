{-|
Module      : AI
Description : AIs for Othello
Copyright   : (c) 2020 Ziling Ouyang
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
        -- ("minimaxStrategy", WithLookahead minMaxAI)
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
greedyHeuristic [] _ = Move (9,9) -- Techincally this is cased below
greedyHeuristic (x:xs) gameState@(GameState _ turn _) = case turn of
    GameOver _  -> error "greedyAI: Invalid Input (Gameover is invalid)"
    Turn _      -> snd (foldr1 maxTuple (map score tupleList))
  where
    -- | Returns a list of Maybe GameState given a egal list of moves
    gameStateList :: [Maybe GameState]
    gameStateList = map (applyMove gameState) (x:xs)
    -- | Returns a list of (Maybe GameState, Move) given [Maybe GameState]
    -- and [Move]
    tupleList :: [(Maybe GameState, Move)]
    tupleList = zip gameStateList (x:xs)

-- Turns a tuple Maybe GameState list into a list of ints and moves
-- Maybe rename this function
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

-- | Definition of a rose tree
data Rose a = Rose a [Rose a]

-- | A game tree for othello
othelloTree :: (Int,Int) -> GameState -> Rose GameState
othelloTree (acc,depth) gameState
--Rose gameState (map (othelloTree (acc+1,depth)) (map (\(Just x) -> x) (map (applyMove gameState) (legalMoves gameState))))
  | acc < depth = Rose gameState (map ((othelloTree (acc+1,depth) . (\(Just x) -> x)) . applyMove gameState) (legalMoves gameState))
  | otherwise = Rose gameState []

-- | Returns a score which rates each board from a given player's perspective
returnScore :: Player -> Board -> Int
returnScore player board
  | player == Player1 = playerScore
  | otherwise = -playerScore
   where
    playerScore = currentScore board Player1 - currentScore board Player2

-- | A function which determines the most optimal move using minmax techniques
-- given GameState and how many layers to check
-- value :: GameState

-- | Code to show RoseTrees in a nicer manner
instance Show a => Show (Rose a) where
  show = unlines . layout
    where
      layout :: Show a => Rose a -> [String]
      layout (Rose v []) = [show v]
      layout (Rose v children) = show v : concatMap (indent . layout) children
      indent :: [String] -> [String]
      indent = map ("  "++)

-- | An example of a list of moves for testing
li :: [Move]
li = [Move (3,2), Move (2,3), Move (3,3)]

-- Testing function: foldr max (0) (map score (map (applyMove (initialState (8,8))) li))
-- Gives out the max int from a list of ints
-- (map score' zip (map (applyMove (initialState (8,8))) li) li))
-- Gives out a list of (int, move)
-- foldr max' (0,Move (1,1)) (map score' zip (map (applyMove (initialState (8,8))) li) li))
-- Gives out the tuple which has the most score advantage (Score, Move)
-- map (applyMove gameState) legalMove gameState

-- (map ((applyMove (GameState bounds turn board)) (x:xs))) 
-- Gives out a list of Maybe GameState

-- map score (map ((applyMove (GameState bounds turn board)) (x:xs)))
-- Returns a list of scores

-- Gives out a tuple of lists given a list and another list
-- zip (map (applyMove (initialState (8,8))) li) li