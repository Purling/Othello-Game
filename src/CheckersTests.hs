{-|
Module      : CheckersTests
Description : Tests for the Checkers game
Copyright   : (c) 2020 The Australian National University
License     : AllRightsReserved
-}
module CheckersTests where

import           Checkers
import           Data.Aeson
import           Dragons.Checkers      ()
import           Dragons.Checkers.Text
import           Testing

checkersTests :: Test
checkersTests = TestGroup "Checkers"
  [ initialStateTests
  , legalMovesTests
  , countPiecesTests
  , applyMoveTests
  , jsonTests
  , moveParsingTests
  ]

initialStateTests :: Test
initialStateTests = TestGroup "initialState"
  [ Test "correct height"
      (assertEqual (length (board (initialState (6,8)))) 8)
  , Test "correct widths"
      (assertEqual (boardWidths (initialState (6, 8))) (replicate 8 6))
  ]

legalMovesTests :: Test
legalMovesTests = TestGroup "legalMoves"
  [ Test "on intialState"
      (assertEqual (legalMoves (initialState (8, 8)))
        -- 2 moves for front-row middle pieces, 1 for edge pieces.
        -- Edge
        [ Move (Location 0 5) (Location 1 4)

        -- Middle
        , Move (Location 2 5) (Location 1 4)
        , Move (Location 2 5) (Location 3 4)
        , Move (Location 4 5) (Location 3 4)
        , Move (Location 4 5) (Location 5 4)
        , Move (Location 6 5) (Location 5 4)
        , Move (Location 6 5) (Location 7 4)
        ])
  ]

countPiecesTests :: Test
countPiecesTests = TestGroup "countPieces"
  [ Test "on small custom board"
      (assertEqual (countPieces (State (Turn Player1) None  (8, 5) testBoard (History 0 [])))
        (7, 4))
  ]

applyMoveTests :: Test
applyMoveTests = TestGroup "applyMove"
  [ Test "capturing on small custom board, turn not changing, counter reset"
      (assertEqual
        (applyMove
          (Move (Location 1 0) (Location 3 2))
          (State (Turn Player2) None  (8, 5) testBoard (History 3 [])))
        (Just (State (Turn Player2)
              (Captor (Location 3 2) [Location 2 1])
              (8, 5) testBoard' (History 0 []))))

  , Test "capturing on small custom board, promotion"
      (assertEqual
        (applyMove
          (Move (Location 3 2) (Location 5 4))
          (State (Turn Player2)
          (Captor (Location 3 2) [Location 2 1])
          (8, 5) testBoard' (History 0 [])))
        (Just (State (Turn Player1)
              None
              (8, 5) testBoard'' (History 0 []))))

  , Test "capturing on small custom board, as king"
      (assertEqual
        (applyMove
          (Move (Location 5 4) (Location 7 2))
          (State (Turn Player2)
          (Captor (Location 5 4) [Location 4 3, Location 2 1])
          (8, 5) testBoard'' (History 0 [])))
        (Just (State (Turn Player1) None (8, 5) testBoard''' (History 0 []))))

  ]

jsonTests :: Test
jsonTests = TestGroup "JSON encode/decode"
  [ Test "simple encode/decode of Move"
      (assertEqual (decode (encode mv)) (Just mv))
  ]
  where mv = Move (Location 5 6) (Location 4 7)

moveParsingTests :: Test
moveParsingTests = TestGroup "move parsing/unparsing"
  [ Test "reading roundtrip"
      (assertEqual (renderMove <$> parseMove st "A6-A5") (Just "A6-A5"))
  , Test "printing roundtrip"
      (assertEqual
        (parseMove st (renderMove (Move (Location 5 4) (Location 4 5))))
        (Just (Move (Location 5 4) (Location 4 5))))
  ]
  where st = initialState (8, 8)

boardWidths :: GameState -> [Int]
boardWidths = map length . board

testBoard :: Board
testBoard = toBoard
  [ "#x#x#x# "
  , " #o#o# #"
  , "#o# #o# "
  , " #X#o#o#"
  , "#o# # # "
  ]

testBoard' :: Board
testBoard' = toBoard
  [ "# #x#x# "
  , " # #o# #"
  , "#o#x#o# "
  , " #X#o#o#"
  , "#o# # # "
  ]
testBoard'' :: Board
testBoard'' = toBoard
  [ "# #x#x# "
  , " # #o# #"
  , "#o# #o# "
  , " #X# #o#"
  , "#o# #X# "
  ]

testBoard''' :: Board
testBoard''' = toBoard
  [ "# #x#x# "
  , " # #o# #"
  , "#o# #o#X"
  , " #X# # #"
  , "#o# # # "
  ]

toBoard :: [String] -> Board
toBoard = (map . map) parsePiece
  where
   parsePiece c = case c of
    ' ' -> Empty
    'x' -> Piece Player2 Pawn
    'X' -> Piece Player2 King
    'o' -> Piece Player1 Pawn
    'O' -> Piece Player1 King
    _   -> Blank
