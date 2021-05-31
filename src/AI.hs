{-|
Module      : AI
Description : AIs for Othello
Copyright   : (c) 2020 Ziling Ouyang
License     : AllRightsReserved
-}
module AI where

import Othello

-- | Definition of a rose tree
data Rose a = Rose a [Rose a]

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
        ("greedyStrategy", NoLookahead greedyAI),
        ("minmax", NoLookahead minMaxAI'),
        ("default", WithLookahead minMaxAI)
      ]

-- | First predefined AI

-- | A very simple AI, which picks the first move returned by the
-- 'legalMoves' function. AIs can rely on the 'legalMoves' list being
-- non-empty; if there were no legal moves, the framework would have
-- ended the game.
firstLegalMove :: GameState -> Move
firstLegalMove st = head (legalMoves st)

-- | AI which has no lookahead and is purely greedy based on coin parity

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
greedyHeuristic [] _ = error "hit" -- Techincally this is cased below
greedyHeuristic (x:xs) gameState@(GameState _ turn _) = case turn of
    GameOver _  -> head (legalMoves gameState)
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

-- | MM AI for Othello with more sophisticated heuristics

-- | A game tree for othello generated up to a given depth
othelloTree :: (Int,Int) -> GameState -> Rose GameState
othelloTree (_,_) gameState@(GameState _ (GameOver _) _) = Rose gameState []
othelloTree (acc,depth) gameState@(GameState bound (Turn player) board) =
  case map (applyMove gameState) (legalMoves gameState) of
  [] -> Rose gameState (map op' (legalMoves opposite))
  _
    | depth == 1 -> Rose gameState
    (map ((`Rose` []) . ((\(Just x) -> x) . applyMove gameState))
    (legalMoves gameState))
    | acc < depth -> Rose gameState (map op (legalMoves gameState))
    | otherwise -> Rose gameState []
  where
      op = (othelloTree (acc+1,depth) . (\(Just x) -> x)) . applyMove gameState
      op' = (othelloTree (acc+1,depth) . (\(Just x) -> x)) . applyMove opposite
      opposite = GameState bound (Turn (otherPlayer player)) board

-- | MM AI that only has lookahead to a certain degree
minMaxAI' :: GameState -> Move
minMaxAI' gameState@(GameState _ (GameOver _) _) = head (legalMoves gameState)
minMaxAI' gameState@(GameState _ (Turn player) _) = getMove gameState
  (getBest (depthRepeat player 4 originalTree))
  where
    originalTree = convertLeaves player (othelloTree (0,4) gameState)

-- | MM AI which has lookahead until time limit is up
minMaxAI :: GameState -> Int -> Move
minMaxAI gameState@(GameState _ (Turn player) _) 1 = getMove gameState
  (getBest (convertLeaves player (othelloTree (0,1) gameState)))
minMaxAI gameState@(GameState _ (GameOver _) _) _ = head (legalMoves gameState)
minMaxAI gameState@(GameState _ (Turn player) _) depth = getMove gameState
  (getBest (depthRepeat player depth originalTree))
  where
    originalTree = convertLeaves player (othelloTree (0,depth) gameState)

-- | Function which returns the best move from a list of moves
getMove :: GameState -> Int -> Move
getMove gameState = getMove' (zip (legalMoves gameState) [1..64 ::Int])

-- | Helper function which generates the best move according to ranking
getMove' :: [(Move,Int)] -> Int -> Move
getMove' [] _ = Move (9,9)
getMove' ((move,index):xs) position
  | index == position = move
  | otherwise = getMove' xs position

-- | Returns the position of the best move in a list
getBest :: Rose (Maybe Int, GameState) -> Int
getBest (Rose (_, _) list) = bestMove (zip list [1..64 ::Int])
  where
    bestMove :: [(Rose (Maybe Int, GameState),Int)] -> Int
    bestMove [] = 0
    bestMove [(_,count)] = count
    bestMove ((Rose (int,_) _,count):[(Rose (counter,_) _,count1)])
      | int >= counter = count
      | otherwise = count1
    bestMove (x@(Rose (int,_) _,count):y@(Rose (counter,_) _,count1):xs) =
      case null xs of
      True
        | int >= counter -> bestMove (x:xs)
        | otherwise -> bestMove (y:xs)
      False
        | int >= counter -> count
        | otherwise -> count1

-- | Function which recurses a leaf value up a tree depth by depth
depthRepeat :: Player -> Int -> Rose (Maybe Int,GameState) ->
   Rose (Maybe Int,GameState)
depthRepeat player depth rTree = case depth of
  2 -> minMaxHeuristic player (0,2) rTree
  _ -> depthRepeat player (depth-1) (minMaxHeuristic player (0,depth) rTree)

-- | Returns a game tree with leaves converted by heuristic function
convertLeaves :: Player -> Rose GameState -> Rose (Maybe Int,GameState)
convertLeaves player rTree@(Rose gameState _) = case rTree of
  Rose (GameState _ _ board) [] -> 
    Rose (Just (returnScore player board),gameState) []
  Rose _ rose -> Rose (Nothing,gameState) (map (convertLeaves player) rose)

-- | Heuristic function for MinMax
minMaxHeuristic :: Player -> (Int,Int) -> Rose (Maybe Int,GameState) -> 
  Rose (Maybe Int,GameState)
minMaxHeuristic player (acc,depth) (Rose (_,gameState) list)
  | (acc == 0) && (0== depth) = Rose (Nothing,gameState) []
  | acc < (depth -1) = Rose (Nothing,gameState) 
    (map (minMaxHeuristic player (acc+1,depth)) list)
  | otherwise = Rose (Just (comparison player 
    (map (\(Rose (Just x,y) _) -> (x,y)) list)),gameState) []

-- | Function which generates the value most likely to be choosen from a 
-- gametree depth based on player
comparison :: Player -> [(Int, GameState)] -> Int
comparison _ [] = 0
comparison _ [(int,_)] = int
comparison player (x:xs) = case x of
  (int,GameState _ (Turn play) _)
   | player == play -> min int (comparison player xs)
   | otherwise -> max int (comparison player xs)
  (_, GameState _ (GameOver (Winner play)) _)
    | player == play -> min 1000000 (comparison player xs)
    | otherwise -> 1000000
  (_, GameState _ (GameOver Draw) _) -> max (-1000000) (comparison player xs)

-- | Function which calculates how many corners are occuppied and returns a 
-- value to score the amount of corners occupied
corner :: Player -> Board -> Int
corner player board = case player of 
  Player1 -> case corners of
    (Just Player1, Just Player1, Just Player1, Just Player1) -> 4
    (_, Just Player1, Just Player1, Just Player1) -> 3
    (Just Player1, _, Just Player1, Just Player1) -> 3
    (Just Player1, Just Player1, _, Just Player1) -> 3
    (Just Player1, Just Player1, Just Player1, _) -> 3
    (_, _, Just Player1, Just Player1) -> 2
    (_, Just Player1, _, Just Player1) -> 2
    (_, Just Player1, Just Player1, _) -> 2
    (Just Player1, _, _, Just Player1) -> 2
    (Just Player1, _, Just Player1, _) -> 2
    (Just Player1, Just Player1, _, _) -> 2
    (Just Player1, _, _, _) -> 1
    (_, Just Player1, _, _) -> 1
    (_, _, Just Player1, _) -> 1
    (_, _, _, Just Player1) -> 1
    _ -> 0
  _ -> case corners of
    (Just Player2, Just Player2, Just Player2, Just Player2) -> 4
    (_, Just Player2, Just Player2, Just Player2) -> 3
    (Just Player2, _, Just Player2, Just Player2) -> 3
    (Just Player2, Just Player2, _, Just Player2) -> 3
    (Just Player2, Just Player2, Just Player2, _) -> 3
    (_, _, Just Player2, Just Player2) -> 2
    (_, Just Player2, _, Just Player2) -> 2
    (_, Just Player2, Just Player2, _) -> 2
    (Just Player2, _, _, Just Player2) -> 2
    (Just Player2, _, Just Player2, _) -> 2
    (Just Player2, Just Player2, _, _) -> 2
    (Just Player2, _, _, _) -> 1
    (_, Just Player2, _, _) -> 1
    (_, _, Just Player2, _) -> 1
    (_, _, _, Just Player2) -> 1
    _ -> 0
  where
    gameState = GameState (8,8) (Turn player) board
    corners = (  pieceAt gameState (0,0)
                 , pieceAt gameState (7,0)
                 , pieceAt gameState (0,7)
                 , pieceAt gameState (7,7))

-- | Returns a score which rates each board from a given player's perspective
returnScore :: Player -> Board -> Int
returnScore player board
  | minieLength + maxieLength /= 0 && maxieCorner + minieCorner /= 0 =
    200*((maxieLength - minieLength) `div` (maxieLength + minieLength))
    + 20 * (maxie - minie) `div` (maxie + minie) 
    + 800 * (maxieCorner - minieCorner) `div` (maxieCorner + minieCorner)
  | maxieCorner + minieCorner /= 0 =
    80 * (maxieCorner - minieCorner) `div` (maxieCorner + minieCorner)
    + 20 * (maxie - minie) `div` (maxie + minie)
  | minieLength + maxieLength /= 0 = 
    200*((maxieLength - minieLength) `div` (maxieLength + minieLength))
    + 20 * (maxie - minie) `div` (maxie + minie)
  | otherwise = 20 * (maxie - minie) `div` (maxie + minie) 
    + 80*corner player board
  where
    minie = currentScore board (otherPlayer player)
    maxie = currentScore board player
    minieLength = length (legalMoves (GameState (8,8) 
      (Turn (otherPlayer player)) board))
    maxieLength = length (legalMoves (GameState (8,8) (Turn player) board))
    minieCorner = corner (otherPlayer player) board
    maxieCorner = corner player board

-- | Code to show RoseTrees in a nicer manner
instance Show a => Show (Rose a) where
  show = unlines . layout
    where
      layout :: Show a => Rose a -> [String]
      layout (Rose v []) = [show v]
      layout (Rose v children) = show v : concatMap (indent . layout) children
      indent :: [String] -> [String]
      indent = map ("  "++)