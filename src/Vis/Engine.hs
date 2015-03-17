module Vis.Engine where

import Settlers

import Control.Monad
import Control.Monad.State

import AsyncPipes
import AsyncPipes.Combinators


data InteractiveState

type EngineMonad = State InteractiveState

type Interaction r = Cond UserIn EngineOut EngineMonad r

data UserIn = Choose Int

-- Number of cards to choose
newtype ForChoice = ForChoice Int

-- Deck of any cards
data Deck = Deck [DeckCard]

data EngineOut = ShowDeck PlayerId ForChoice Deck
