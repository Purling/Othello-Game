{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Dragons.Checkers
Description : Checkers-specific things students don't need to see
Copyright   : (c) 2020 The Australian National University
License     : AllRightsReserved

This module collects functions and instances for Checkers-specific data
structures (so they don't belong in the generic game framework), but
are also implementation details that students don't need to concern
themselves with.
-}
module Dragons.Checkers where

import AI
import Checkers
import Data.Aeson
import Dragons.Game

toAITable :: [(String, AIFunc)] -> [(String, GenericAIFunc GameState Move)]
toAITable = (fmap . fmap) toGenericAIFunc
  where
    toGenericAIFunc :: AIFunc -> GenericAIFunc GameState Move
    toGenericAIFunc aiFunc st = case aiFunc of
      NoLookahead f -> [f st]
      WithLookahead f -> map (f st) [1..]

rules1100 :: GameRules GameState Move
rules1100 = GameRules
  { gameInitialState = initialState (8, 8)
  , gameGetTurn = turn
  , gameApplyMove = applyMove
  }

-- How to turn move types to and from JSON. Best practice is
-- to define instances next to either the data type or the
-- typeclass. These are "orphan" instances, and normally poor
-- practice, but we don't want to have too much mysterious code in
-- files that students need to read.

instance FromJSON Move where
  parseJSON = withObject "move" $ \o -> Move
    <$> o .: "from"
    <*> o .: "to"

instance FromJSON Location where
  parseJSON = withObject "location" $ \o -> Location
    <$> o .: "x"
    <*> o .: "y"

instance ToJSON Move where
  toJSON (Move from to) = object
    [ "from" .= from
    , "to" .= to
    ]

instance ToJSON Location where
  toJSON (Location x y) = object
    [ "x" .= x, "y" .= y ]

instance ToJSON Captor where 
  toJSON c = case c of 
    None -> object []
    Captor loc capt -> object
       [ "loc" .= loc, "captured" .= capt ]

instance ToJSON GameState where
  toJSON (State t c bnd brd (History n h)) = object
    [ "turn" .= t
    , "captured" .= c
    , "bounds" .= bnd
    , "board" .= jsonBoard brd
    , "counter" .= n
    , "history" .= map jsonBoard h
    ]
    where
      jsonBoard :: Board -> Value
      jsonBoard = String . (foldMap . foldMap) (\case
        Piece Player1 Pawn -> "1"
        Piece Player2 Pawn -> "2"
        Piece Player1 King -> "!"
        Piece Player2 King -> "@"
        Empty -> " "
        Blank -> "#")
