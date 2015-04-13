module Settlers.Game where

import Settlers.Engine
import Settlers.Core

import Control.Applicative
import Control.Monad
import Control.Monad.State
import qualified Data.Sequence as S

import Game

import Pipes
import Pipes.PseudoParal
import qualified Pipes.Prelude as PP

import qualified Engine as E
import qualified Settlers.Const as C
import EngineMonad

playTurn :: Monad m => EngineAction m Bool
playTurn = return False

initPlayer :: Monad m => PlayerId Settlers -> EngineAction m Bool
initPlayer p = E.withPlayerMeta p interaction
  where
    interaction = do
      g <- get
      let deck = fmap DHandCard $ gsAbilityDecks g `S.index` fromEnum p
      E.pipeToMeta $ yield $ ShowDeck deck $ ForChoice C.initialCards
      timer <- lift $ setTimeout (Delay 60)
      cards <- E.withTimer timer $ awaitList C.initialCards
      return True

startGame :: Monad m => EngineAction m Bool
startGame = do
  (ok1, ok2) <- paralBoth (initPlayer Player1) (initPlayer Player2)
  return $ ok1 && ok2

playGame :: Monad m => EngineAction m ()
playGame = do
  isOk <- startGame
  when isOk $ while playTurn

--Utilities
while :: Monad m => m Bool -> m ()
while cond = do
  continue <- cond
  when continue $ while cond

awaitList :: Monad m => Int -> Pipe i o m [i]
awaitList 0 = return []
awaitList n = do
  x <- await
  (x : ) <$> awaitList (n - 1)
