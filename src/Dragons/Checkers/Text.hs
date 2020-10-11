{-|
Module      : Dragons.Checkers.Text
Description : Text interface for the Checkers game
Copyright   : (c) 2020 The Australian National University
License     : AllRightsReserved
-}
module Dragons.Checkers.Text where

import Checkers
import Data.Char
import Data.Maybe
import Dragons.Checkers ()
import Dragons.Game
import Dragons.Game.UI.Text as UI

textUI :: GameConfig GameState Move -> GameUI GameState Move
textUI config = UI.textUI config $ TextUI
  { textRenderState = renderState
  , textReadMove = readMove
  }

renderState :: GameState -> String
renderState st = unlines $ catMaybes
  [ renderTurn (turn st)
  , Just $ renderBoard $ stateToBoard st
  ]

renderTurn :: Turn -> Maybe String
renderTurn t = case t of
  Turn Player1 -> Just "Player 1 (O) to move"
  Turn Player2 -> Just "Player 2 (X) to move"
  GameOver _ -> Nothing

stateToBoard :: GameState -> Board
stateToBoard st@(State t c _ b _) = case (t,c) of
 (Turn p, Captor _ cs) -> addCaptives (otherPlayer p)
  where 
    addCaptives :: Player -> Board
    addCaptives op = board (foldr ((flip adjustSquareAt) (const (Piece op Pawn))) st cs) 
 _ -> b

renderBoard :: Board -> String
renderBoard brd = unlines $
  heading : zipWith (:) ['0'..'9'] ((map . map) renderSquare brd)
  where
    heading :: String
    heading = ' ' : ['A'..'H']

    renderSquare :: Square -> Char
    renderSquare sq = case sq of
      Empty -> ' '
      Blank -> '#'
      Piece Player1 King -> 'O'
      Piece Player2 King -> 'X'
      Piece Player1 Pawn -> 'o'
      Piece Player2 Pawn -> 'x'
     
-- | Ask for a move, check that it's sensible, and if it isn't, ask again.
readMove :: GameState -> Maybe (Player, Move) -> IO Move
readMove st _ = loop
  where
    loop = do
      putStrLn $ "Enter a move. Example: " ++ renderMove (head (legalMoves st))
      line <- getLine
      case parseMove st line of
        Nothing -> errLoop "Malformed move, try again."
        Just mv
          | mv `elem` legalMoves st -> pure mv
          | get st (moveFrom mv) /= Just (Piece p Pawn) &&
            get st (moveFrom mv) /= Just (Piece p King) ->
              errLoop "You don't own a piece at the move's origin."
          | get st (moveTo mv) == Just Blank -> 
              errLoop "You must stay on the dark tiles!"
          | get st (moveTo mv) /= Just Empty ->
              errLoop "Destination square not empty"
          | (moveTo mv) `elem` captured st ->
              errLoop "Destination square is not yet empty."
          | otherwise -> errLoop "Not a valid move."

    errLoop s = putStrLn s *> loop

    Turn p = turn st

-- | Parse a 'String' that should describe a 'Move', if it makes sense
-- for where we are in the current game.
parseMove :: GameState -> String -> Maybe Move
parseMove st s = case map toUpper s of
  [ff, fr, '-', tf, tr] ->
    case (fromFile ff, fromRank fr, fromFile tf, fromRank tr) of
      (Just fx, Just fy, Just tx, Just ty) ->
        Just (Move (Location fx fy) (Location tx ty))
      _ -> Nothing

    where
      fromFile r
        | x >= 0 && x < w = Just x
        | otherwise = Nothing
        where
          x = ord r - ord 'A'

      fromRank f
        | y >= 0 && y < h = Just y
        | otherwise = Nothing
        where
          y = ord f - ord '0'

      (w,h) = bounds st
  _ -> Nothing

renderMove :: Move -> String
renderMove (Move (Location fx fy) (Location tx ty))
  = fs ++ "-" ++ ts
  where
    fs = [ff, fr]
    ts = [tf, tr]

    ff = toFile fx
    fr = toRank fy
    tf = toFile tx
    tr = toRank ty

    toRank n = ['0'..'9'] !! n
    toFile n = ['A'..'Z'] !! n
