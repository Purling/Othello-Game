{-|
Module      : AITests
Description : Tests for your AI functions
Copyright   : (c) 2020 Your Name Here
License     : AllRightsReserved
-}
module AITests where

import           AI
import           Othello
import           Testing
import OthelloTests

aiTests :: Test
aiTests = TestGroup "AI"
  [ cornerTest,
    othelloTreeTest,
    minMaxAITest,
    getMoveTest,
    getMove'Test,
    depthRepeatTest,
    comparisonTest

  ]

othelloTreeTest :: Test
othelloTreeTest = Test "othelloTree"
  (assertEqual (roseFlatten(othelloTree (0,2) (GameState (8,8) (Turn Player1)
  (OthelloTests.initialBoardBig))))
  (roseFlatten(othelloTree (0,2) (initialState (8,8)))))

minMaxAITest :: Test
minMaxAITest = Test "minMaxAI"
  (assertNotEqual (minMaxAI initialStateBig 3) (Move (8,8)))

getMoveTest :: Test
getMoveTest = Test "getMove"
  (assertEqual (getMove initialStateBig 2)
  ((\(_:y:_) -> y) (legalMoves initialStateBig)))

getMove'Test :: Test
getMove'Test = Test "getMove'"
  (assertEqual (getMove' [(Move (1,1),1),(Move (2,3),2),(Move (2,3),3)] 2)
  (Move (2,3)))

depthRepeatTest :: Test
depthRepeatTest = Test "depthRepeat"
  (assertNotEqual (roseFlatten(depthRepeat Player1 2 (convertLeaves Player1
  (othelloTree (0,2) (initialState (8,8))))))
  (roseFlatten(convertLeaves Player2 (othelloTree (0,3) (initialState (8,8))))))

comparisonTest :: Test
comparisonTest = Test "comparison"
  (assertEqual (comparison Player1 [(1,initialStateBig),(1,(GameState (8,8) 
  (GameOver (Winner Player1)) initialBoardBig))]) (1))

cornerTest :: Test
cornerTest = Test "corner"
  (assertEqual (corner Player1 ((\(GameState _ _ x) -> x) cornerTestState))
   2)

returnScoreTest :: Test
returnScoreTest = Test "returnScore"
  (assertNotEqual (returnScore (Player1) initialBoardBig) 
  (returnScore (Player2) initialBoardBig))

cornerTestState :: GameState
cornerTestState = GameState (8,8) (Turn Player1) board
    where
        board = OthelloTests.readBoard boardString
        boardString =
            ["O......O"
            ,"........"
            ,"........"
            ,"........"
            ,"........"
            ,"........"
            ,"........"
            ,"X......X"]

-- | Helper function for tests
roseFlatten :: Rose a -> [a]
roseFlatten rTree = case rTree of
    Rose a [] -> [a]
    Rose a (xs) -> a:concat (map roseLeaves (xs))

roseLeaves :: Rose a -> [a]
roseLeaves rTree = case rTree of
    Rose a [] -> [a]
    Rose _ (xs) -> concat (map roseLeaves (xs))

