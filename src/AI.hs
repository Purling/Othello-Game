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
        ("greedyStrategy", NoLookahead greedyAI),
        ("default", NoLookahead minMaxAI)
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

-- | A game tree for othello generated up to a given depth
othelloTree :: (Int,Int) -> GameState -> Rose GameState
othelloTree (acc,depth) gameState
--Rose gameState (map (othelloTree (acc+1,depth)) (zip (map (\(Just x) -> x) (map (applyMove gameState) (legalMoves gameState)))) (legalMoves gameState))
  | acc < depth = Rose gameState (map op (legalMoves gameState))
  | otherwise = Rose gameState []
    where
      op = (othelloTree (acc+1,depth) . (\(Just x) -> x)) . applyMove gameState

-- case applyMove gameState of []

-- | A game tree for othello generated up to a given depth
othelloTree' :: (Int,Int) -> GameState -> Rose GameState
othelloTree' (_,_) gameState@(GameState _ (GameOver _) _) = Rose gameState []
othelloTree' (acc,depth) gameState@(GameState bound (Turn player) board) = case map (applyMove gameState) (legalMoves gameState) of
  [] -> Rose gameState (map op' (legalMoves opposite))
  _
--Rose gameState (map (othelloTree (acc+1,depth)) (map (\(Just x) -> x) (map (applyMove gameState) (legalMoves gameState)))))
    | acc < depth -> Rose gameState (map op (legalMoves gameState))
    | otherwise -> Rose gameState []
  where
        op = (othelloTree' (acc+1,depth) . (\(Just x) -> x)) . applyMove gameState
        op' = (othelloTree' (acc+1,depth) . (\(Just x) -> x)) . applyMove opposite
        opposite = GameState bound (Turn (otherPlayer player)) board

testState :: GameState
testState = GameState (3,3) (Turn Player1) (initialBoard' (3,3))

initialBoard' :: Bounds -> Board
initialBoard' (bx, by) = map makeRow [0..by-1]
  where
    makeRow :: Int -> [Maybe Player]
    makeRow y = map (\x -> pieceOn (x, y)) [0..bx-1]

    pieceOn :: Position -> Maybe Player
    pieceOn pos
      | pos == (0, 1) = Just Player2 
      | pos == (1,1) = Just Player1
      | pos == (2,2) = Just Player1
      | otherwise = Nothing

-- | Remember to create a function which returns the original player to convertLeaves
minMaxAI :: GameState -> Move
minMaxAI gameState@(GameState _ (GameOver _) _) = head (legalMoves gameState)
minMaxAI gameState@(GameState _ (Turn player) _) = getMove gameState (getBest (repeat' player 4 (convertLeaves player (othelloTree' (0,4) gameState))))
  
-- minMaxAI :: GameState -> Int -> Move
-- minMaxAI gameState depth = getMove gameState depth --(getBest (repeat' depth (convertLeaves (othelloTree' (0,depth) gameState))))

getMove :: GameState -> Int -> Move
getMove gameState position = getMove' (zip (legalMoves gameState) [1..64 ::Int]) position

getMove' :: [(Move,Int)] -> Int -> Move
getMove' [] _ = error "Hit Getmove'"
getMove' ((move,index):xs) position
  | index == position = move 
  | otherwise = getMove' xs position
  
getBest :: Rose (Maybe Int, GameState) -> Int
getBest (Rose (_, _) list) = bestMove (zip list [1..64 ::Int])
  where
    bestMove :: [(Rose (Maybe Int, GameState),Int)] -> Int
    bestMove [(_,count)] = count
    bestMove (((Rose (int,_) _),count):[((Rose (counter,_) _),count1)])
      | int >= counter = count 
      | otherwise = count1
    bestMove (x@((Rose (int,_) _),count):y@((Rose (counter,_) _),count1):xs) = case null(xs) of
      True 
        | int >= counter -> bestMove (x:xs) 
        | otherwise -> bestMove (y:xs)
      False 
        | int >= counter -> count 
        | otherwise -> count1

repeat' :: Player -> Int -> Rose (Maybe Int,GameState) -> Rose (Maybe Int,GameState)
repeat' player depth rTree = case depth of
  2 -> minMaxHeuristic player (0,2) rTree
  _ -> repeat' player (depth-1) (minMaxHeuristic player (0,depth) rTree)

-- | Returns a game tree with leaves converted by heuristic function
convertLeaves :: Player -> Rose GameState -> Rose (Maybe Int,GameState)
convertLeaves player rTree@(Rose gameState _) = case rTree of
  Rose (GameState _ _ board) [] -> Rose (Just (returnScore player board),gameState) []
  Rose _ rose -> Rose (Nothing,gameState) (map (convertLeaves player) rose)

-- | Heuristic function for MinMax
minMaxHeuristic :: Player -> (Int,Int) -> Rose (Maybe Int,GameState) -> Rose (Maybe Int,GameState)
minMaxHeuristic player (acc,depth) (Rose (_,gameState) list)
  | (acc == 0) && (0== depth) = Rose (Nothing,gameState) []
  | acc < (depth -1) = Rose (Nothing,gameState) (map (minMaxHeuristic player (acc+1,depth)) list)
  | otherwise = Rose (Just (comparison player (map (\(Rose (Just x,y) _) -> (x,y)) list)),gameState) []

-- convertLeaves (othelloTree' (0,3) (initialState (8,8)))

-- | Case player in this.
comparison :: Player -> [(Int, GameState)] -> Int
comparison _ [] = 0
comparison _ [(int,_)] = int
comparison player (x:xs) = case x of
  (int,GameState _ (Turn play) _)
   | player == play -> min int (comparison player xs)
   | otherwise -> max int (comparison player xs)
    --Are the below ever hit?
  (_, GameState _ (GameOver (Winner play)) _) 
    | player == play -> min 65 (comparison player xs)
    | otherwise -> max 65 (comparison player xs)
  (_, GameState _ (GameOver Draw) _) -> max (-65) (comparison player xs)

-- (int,gameState@(GameState _ turn _))

-- | Could make the opposition occupied corners negative
corner :: Player -> Board -> Int
corner player board 
    | corners == (Just player, Just player, Just player, Just player) = 4
    | corners == (Nothing, Just player, Just player, Just player) = 3
    | corners == (Just player, Nothing, Just player, Just player) = 3
    | corners == (Just player, Just player, Nothing, Just player) = 3
    | corners == (Just player, Just player, Just player, Nothing) = 3
    | corners == (Nothing, Nothing, Just player, Just player) = 2
    | corners == (Nothing, Just player, Nothing, Just player) = 2
    | corners == (Nothing, Just player, Just player, Nothing) = 2
    | corners == (Just player, Nothing, Nothing, Just player) = 2
    | corners == (Just player, Nothing, Just player, Nothing) = 2
    | corners == (Just player, Just player, Nothing, Nothing) = 2
    | corners == (Just player, Nothing, Nothing, Nothing) = 1
    | corners == (Nothing, Just player, Nothing, Nothing) = 1
    | corners == (Nothing, Nothing, Just player, Nothing) = 1
    | corners == (Nothing, Nothing, Nothing, Just player) = 1
    | otherwise = 0

-- corner player board = case player of 
--   Player1 -> case corners of
--     (Just Player1, Just Player1, Just Player1, Just Player1) -> 4
--     (_, Just Player1, Just Player1, Just Player1) -> 3
--     (Just Player1, _, Just Player1, Just Player1) -> 3
--     (Just Player1, Just Player1, _, Just Player1) -> 3
--     (Just Player1, Just Player1, Just Player1, _) -> 3
--     (_, _, Just Player1, Just Player1) -> 2
--     (_, Just Player1, _, Just Player1) -> 2
--     (_, Just Player1, Just Player1, _) -> 2
--     (Just Player1, _, _, Just Player1) -> 2
--     (Just Player1, _, Just Player1, _) -> 2
--     (Just Player1, Just Player1, _, _) -> 2
--     (Just Player1, _, _, _) -> 1
--     (_, Just Player1, _, _) -> 1
--     (_, _, Just Player1, _) -> 1
--     (_, _, _, Just Player1) -> 1
--     _ -> 0
--   _ -> case corners of
--     (Just Player2, Just Player2, Just Player2, Just Player2) -> 4
--     (_, Just Player2, Just Player2, Just Player2) -> 3
--     (Just Player2, _, Just Player2, Just Player2) -> 3
--     (Just Player2, Just Player2, _, Just Player2) -> 3
--     (Just Player2, Just Player2, Just Player2, _) -> 3
--     (_, _, Just Player2, Just Player2) -> 2
--     (_, Just Player2, _, Just Player2) -> 2
--     (_, Just Player2, Just Player2, _) -> 2
--     (Just Player2, _, _, Just Player2) -> 2
--     (Just Player2, _, Just Player2, _) -> 2
--     (Just Player2, Just Player2, _, _) -> 2
--     (Just Player2, _, _, _) -> 1
--     (_, Just Player2, _, _) -> 1
--     (_, _, Just Player2, _) -> 1
--     (_, _, _, Just Player2) -> 1
--     _ -> 0
  where
    gameState = GameState (8,8) (Turn player) board
    corners = (  pieceAt gameState (0,0)
                 , pieceAt gameState (7,0)
                 , pieceAt gameState (0,7)
                 , pieceAt gameState (7,7))

-- | Rose tree for testing purposes
testRose :: Rose Int
testRose = Rose 5 [Rose 3 [Rose 1 [], Rose 4 []], Rose 7 [], Rose 4 []]  

-- | Returns a score which rates each board from a given player's perspective
returnScore :: Player -> Board -> Int
returnScore player board
  | minieLength + maxieLength /= 0 = 80*((maxieLength - minieLength) `div` (maxieLength + minieLength))
    + 20 * (maxie - minie) `div` (maxie + minie) + 200*corner player board
  | otherwise = 20 * (maxie - minie) `div` (maxie + minie) + 200*corner player board
  where
    minie = currentScore board (otherPlayer player)
    maxie = currentScore board player
    minieLength = length (legalMoves (GameState (8,8) (Turn (otherPlayer player)) board))
    maxieLength = length (legalMoves (GameState (8,8) (Turn player) board))

-- returnScore player board
--   | minieLength + maxieLength /= 0 && maxieCorner + minieCorner /= 0 =
--     200*((maxieLength - minieLength) `div` (maxieLength + minieLength))
--     + 20 * (maxie - minie) `div` (maxie + minie) 
--     + 800 * (maxieCorner - minieCorner) `div` (maxieCorner + minieCorner)
--   | maxieCorner + minieCorner /= 0 =
--     80 * (maxieCorner - minieCorner) `div` (maxieCorner + minieCorner)
--     + 20 * (maxie - minie) `div` (maxie + minie)
--   | minieLength + maxieLength /= 0 = 
--     200*((maxieLength - minieLength) `div` (maxieLength + minieLength))
--     + 20 * (maxie - minie) `div` (maxie + minie)
--   | otherwise = 20 * (maxie - minie) `div` (maxie + minie) + 80*corner player board
--   where
--     minie = currentScore board (otherPlayer player)
--     maxie = currentScore board player
--     minieLength = length (legalMoves (GameState (8,8) (Turn (otherPlayer player)) board))
--     maxieLength = length (legalMoves (GameState (8,8) (Turn player) board))
--     minieCorner = corner (otherPlayer player) board
--     maxieCorner = corner player board
    
-- (repeat' (Player1) 11 (convertLeaves (Player1) (othelloTree' (0,3) (initialState (4,4))))))
-- (repeat' (Player1) 3 (convertLeaves (Player1) (othelloTree' (0,3) (initialState (8,8)))))

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