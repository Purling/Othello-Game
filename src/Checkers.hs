{-|
Module      : Checkers
Description : Implementation of the Checkers board game
Copyright   : (c) 2020 The Australian National University
License     : AllRightsReserved
-}
module Checkers where

import           Control.DeepSeq
import           Data.List
import           GHC.Generics    (Generic)

-- | The game state contains whose turn it is, the board size, the
-- current board, and a history of previous boards to detect loops. If
-- the game returns to a previous board setup, we assume that we're
-- stuck in a loop and declare the game a draw. We only start tracking
-- history when players make non-duplicating moves, to limit the total
-- amount of data carried around.
data GameState = State Turn Captor (Int,Int) Board History
  deriving (Eq, Show)

turn :: GameState -> Turn
turn (State t _ _ _ _) = t

adjustTurn :: (Turn -> Turn) -> GameState -> GameState
adjustTurn f (State t _ bnd brd h) = State (f t) None bnd brd h

setTurn :: Turn -> GameState -> GameState
setTurn t = adjustTurn (const t)

bounds :: GameState -> (Int, Int)
bounds (State _ _ bnd _ _) = bnd

board :: GameState -> Board
board (State _ _ _ b _) = b

setBoard :: Board -> GameState -> GameState
setBoard b (State t c bnd  _ h) = State t c bnd b h

-- | The history of previous boards that we check for loops. Note that
-- this is not a full history going back to the start of the game; we
-- only add to the history when a kinged piece moves, as other pieces
-- cannot move to a place they have previously been. We clear the
-- history when an unkinged piece moves, or whenever a piece is taken.
-- In accordance with the rules of checkers, we also count the number
-- of turns since a promotion or capture. (Which provides an upper
-- bound for the length of the list of boards.)
history :: GameState -> History
history (State _ _ _ _ h) = h

captured :: GameState -> [Location]
captured (State _ c _  _ _ ) = case c of
 None        -> []
 Captor _ cs -> cs

captor :: GameState -> Maybe Location
captor (State _ c _ _ _) = case c of
 None         -> Nothing
 Captor loc _ -> Just loc

adjustHistory :: (Int -> Int) -- ^Function to update counter
   -> ([Board] -> [Board]) -- ^Function to update board list
   -> GameState -> GameState
adjustHistory i f (State t c bnd brd (History n h)) =
               State t c bnd brd (History (i n) (f h))

clearHistory :: GameState -> GameState
clearHistory = adjustHistory (const 0) (const [])

adjustCaptor :: Location
   -> ([Location] -> [Location])
   -> GameState -> GameState
adjustCaptor l f (State t c bnd brd h) = case c of
 None           -> State t (Captor l (f [])) bnd brd h
 Captor _ taken -> State t (Captor l (f taken)) bnd brd h

clearCaptor :: GameState -> GameState
clearCaptor (State t _ bnd brd h) = State t None bnd brd h

-- Only valid for boards of dimensions (2n,2m) with n,m > 2,
initialState :: (Int, Int) -> GameState
initialState (w,h) = State (Turn Player1) None (w,h) blocks (History 0 [])
    where
      blocks = take (h `div` 2 - 1)
               (concat [ [take w $ tail light, take w light] | _ <- ['1'..]])
              ++ [take w empty, take w $ tail empty]
              ++ take (h `div` 2 - 1)
                 (concat [ [take w dark, take w $ tail dark] | _ <- ['1'..]])
      empty  = concat [ [Empty, Blank] | _ <- ['1'..] ]
      dark   = concat [ [Piece Player1 Pawn, Blank] | _ <- ['1'..] ]
      light  = concat [ [Piece Player2 Pawn, Blank] | _ <- ['1'..] ]

-- | A player is either player 1 or 2.
data Player = Player1 | Player2 deriving (Eq, Show)

otherPlayer :: Player -> Player
otherPlayer p = case p of
  Player1 -> Player2
  Player2 -> Player1

data Turn = Turn Player | GameOver Outcome deriving (Eq, Show)
data Outcome = Winner Player | Draw deriving (Eq, Show)

-- | A location on the board is an X then a Y coordinate.
data Location = Location Int Int deriving (Eq, Generic, NFData, Show)

-- | the board history holds the number of turns since a capture or
-- promotion, and a list of previous boards which could possibly be
-- repeated.  (if a man moves it cannot move back, so in this case the
-- boards are cleared, but the counter is still incremented
data History = History Int [Board] deriving (Eq, Show)


-- | If the current move is in a move sequence, then there must be a
-- "captor", i.e. the piece that is making a string of captures.  We
-- record the captor's current location, and the list of pieces that
-- the captor has captured.
data Captor = None | Captor Location [Location] deriving (Eq, Show)

-- | Is the 'Location' on our board?
onBoard :: GameState -> Location -> Bool
onBoard st (Location x y) = x >= 0 && x < w && y >= 0 && y < h
  where (w, h) = bounds st


allLocations :: GameState -> [Location]
allLocations st =
  [ Location x y | y <- [0..(h - 1)], x <- [0..(w - 1)] ]
  where (w, h) = bounds st

-- | A square on the board.
data Square
  = Piece Player PieceType -- ^ Occupied by a player's piece.
  | Empty -- ^ Empty space.
  | Blank -- ^ Unoccupiable space.
  deriving (Eq, Show)

-- | The different types of pieces.
data PieceType = Pawn | King
  deriving (Eq, Show)

-- | A 'Board' always consists of 'gameHeight' rows, each of length
-- 'gameWidth'. The top-left corner is @'Location' 0 0@.
type Board = [[Square]]

get :: GameState -> Location -> Maybe Square
get st (Location x y)
  | onBoard st (Location x y) = Just (board st !! y !! x)
  | otherwise = Nothing

adjustSquareAt :: Location -> (Square -> Square) -> GameState -> GameState
adjustSquareAt loc@(Location x y) f st = case get st loc of
  Nothing -> st
  Just s -> st'
    where
      st' = setBoard b' st

      b' = beforeRows ++ [changedRow] ++ afterRows
      changedRow = beforeSquares ++ [f s] ++ afterSquares

      (beforeSquares, _:afterSquares) = splitAt x changingRow
      (beforeRows, changingRow:afterRows) = splitAt y (board st)

setSquare :: Location -> Square -> GameState -> GameState
setSquare loc s = adjustSquareAt loc (const s)

clearSquare :: Location -> GameState -> GameState
clearSquare loc = setSquare loc Empty

-- | Move your piece from the first location to the second, which must
-- be empty. A valid move is a diagonal step forward for a man, or a
-- diagonal step in any direction for a king. Or a single capture,
-- where you jump over an opponents piece. In this game, each capture
-- in a sequence is treated as a different move, on the same turn.
data Move = Move Location Location
  deriving (Eq, Generic, NFData, Show)

moveFrom :: Move -> Location
moveFrom (Move from _) = from

moveTo :: Move -> Location
moveTo (Move _ to) = to

data MoveType = Step | Capture Location deriving (Eq, Show)

classifyMove :: Move -> Maybe MoveType
classifyMove (Move (Location x1 y1) (Location x2 y2))
    | (abs (x1 - x2) == 2) && (abs (y1 - y2) == 2) =
        Just $ Capture $ Location ((x1 + x2) `div` 2) ((y1 + y2) `div` 2)
    | (abs (x1 - x2) == 1) && (abs (y1 - y2) == 1) = Just Step
    | otherwise = Nothing

applyMove :: Move -> GameState -> Maybe GameState
applyMove mv@(Move from to) st = case turn st of
  -- Game must not yet be over
  Turn player -> case get st from of
    -- "From" location must name one of our pieces.
    Just (Piece p s)
      | p == player -> case get st to of
          -- "To" location must be vacant.
          Just Empty -> case classifyMove (Move from to) of
            Just Step -> Just (nextTurn st')
            Just (Capture _) -> case legalMoves st' of
                [] -> Just (nextTurn st')
                _  -> Just (if s == promote s then st' else nextTurn st')
            Nothing -> Nothing
          _ -> Nothing
          where st' = movePiece p s st
    _ -> Nothing
  GameOver _ -> Nothing

  where
   promote :: PieceType -> PieceType
   promote s = case to of
      (Location _ y)
            | y == 0 || y == (snd (bounds st) - 1) -> King
            | otherwise -> s

   historyCase s =  case (s, promote s) of
     (Pawn, King) -> clearHistory
     (Pawn, _) -> adjustHistory (+1) (const [])
     (King, _) -> adjustHistory (+1) (board st:)

   movePiece :: Player -> PieceType -> GameState -> GameState
   movePiece p s = case classifyMove mv of
     Just (Capture loc) -> compose
           [ setSquare to (Piece p (promote s))
           , clearHistory
           , adjustCaptor to (loc:)
           , clearSquare loc
           , clearSquare from
           ]
     Just Step -> compose
                [ historyCase s
                , setSquare to (Piece p (promote s))
                , clearSquare from
                ]
     Nothing -> id

-- | Set the next turn in the game state:
--
-- 1. If the current board exists in our history, it's a draw.
-- 2. If the other player can make a move, it's their turn.
-- 3. If the other player cannot, the game is over; tally the scores.
nextTurn :: GameState -> GameState
nextTurn st = case history st of
  History n boards
    | board st `elem` boards || n >= 40 -> setTurn (GameOver Draw) st
    | otherwise -> case (legalMoves st', turn st) of
      ([], Turn p) -> setTurn (GameOver (Winner p)) st
      _            -> st'
  where
    st' = adjustTurn next st

    next :: Turn -> Turn
    next trn = case trn of
      Turn p     -> Turn (otherPlayer p)
      GameOver _ -> trn

-- | This is used to display the score in the CodeWorld game, and not
-- any of the game logic.  if you wish, you may alter this function
-- for your AI to use.
countPieces :: GameState -> (Int, Int)
countPieces st = foldl' count (0, 0) (concat (board st))
  where
    count :: (Int, Int) -> Square -> (Int, Int)
    count (p1,p2) sq = case sq of
      Piece Player1 _ -> (p1 + 1, p2)
      Piece Player2 _ -> (p1, p2 + 1)
      _               -> (p1, p2)

-- | Return the list of legal moves for the current player.
legalMoves :: GameState -> [Move]
legalMoves st = case turn st of
  GameOver _ -> []
  Turn p -> case captor st of
             Nothing -> case captures of
                      [] -> steps
                      -- If it is possible to capture, you must.
                      _ -> captures
             -- If you have already made a capture, then your
             -- `legalMoves` are subsequent captures with that piece.
             Just loc -> map (Move loc) (capturePoints loc)
    where
      captures = concat 
         (zipWith (moves capturePoints) (concat (board st)) (allLocations st))

      steps = concat 
         (zipWith (moves movePoints) (concat (board st)) (allLocations st))

      moves :: (Location -> [Location]) -> Square -> Location -> [Move]
      moves lf sq loc = case sq of
        Piece piece _ | piece == p ->
         map (Move loc) (lf loc)
        _ -> []

      pieceType :: Location -> Maybe PieceType
      pieceType loc = case get st loc of
       Just (Piece _ s) -> Just s
       _                -> Nothing

      -- Locations to which a capture can be made
      capturePoints :: Location -> [Location]
      capturePoints loc@(Location x y) =
        [ Location (x + x') (y + y')
        | x' <- [-2,2]
        , y' <- [-2,2]
        , isOpponentPiece (get st (Location (x + (x' `div` 2)) (y + (y' `div` 2))))
        , get st (Location (x + x') (y + y')) == Just Empty
        , Location (x + x') (y + y') `notElem` captured st
        , (pieceType loc /= Just Pawn) || forward p y'
        ]

      -- Possible values of opponents pieces
      isOpponentPiece :: Maybe Square -> Bool
      isOpponentPiece mp = mp `elem` map (Just . Piece (otherPlayer p)) [Pawn, King]

      -- Locations to which a standard move can be made
      movePoints :: Location -> [Location]
      movePoints loc@(Location x y) =
        [ Location (x + x') (y + y')
        | x' <- [-1,1]
        , y' <- [-1,1]
        , get st (Location (x + x') (y + y')) == Just Empty
        , (pieceType loc /= Just Pawn) || forward p y'
        ]

-- | What direction is "forwards" for player p
forward :: Player -> Int -> Bool
forward p = case p of
 Player1 -> (< 0)
 Player2 -> (> 0)

-- | Given a list of functions, compose them all together.
compose :: [a -> a] -> a -> a
compose = foldr (.) id
