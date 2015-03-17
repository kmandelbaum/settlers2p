{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, GADTs, DataKinds #-}
module Engine where

import Game

import AsyncPipes
import AsyncPipes.Combinators

import Control.Monad
import Control.Monad.State

import Data.Sequence


newtype ForChoice = ForChoice Int

data DataFromPlayer = PlayerChoice Int

data EngineIn g = FromPlayer (PlayerId g) DataFromPlayer

data DataToPlayer g = 
  ShowDeck (Seq (DeckCard g)) ForChoice |
  UpdateState (VisibleState g) |
  Message String

data EngineOut g = ToPlayer (PlayerId g) (DataToPlayer g)

type EngineAction g r = Cond (EngineIn g) (EngineOut g) (EngineMonad g) r

type EngineMonad g = State (GameState g)
