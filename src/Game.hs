{-# LANGUAGE TypeFamilies #-}
module Game where

class Game g where
  type GameState g
  type PlayerId g
  type DeckCard g
  type VisibleState g
