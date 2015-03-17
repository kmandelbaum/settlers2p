{-# LANGUAGE TypeFamilies #-}
module Settlers.GameInstances where

import Game
import qualified Settlers as S

data Settlers

instance Game Settlers where
  type GameState Settlers = S.GameState
  type PlayerId Settlers = S.PlayerId
  type DeckCard Settlers = S.DeckCard
  type VisibleState Settlers = S.VisibleState
