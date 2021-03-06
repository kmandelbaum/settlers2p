{-# LANGUAGE FlexibleContexts, Rank2Types #-}
module Settlers.Game where

import Data.Maybe

import Settlers.Engine
import Settlers.Core
import Settlers.GamePure

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader

import qualified Data.Sequence as S

import Pipes
import Pipes.PseudoParal
import qualified Pipes.Prelude as PP

import qualified Engine as E
import EngineMonad (setTimeout, killTimer, Delay(..))
import Control.Lens

playTurn :: Act Bool
playTurn = return False

initPlayer :: PId -> Act Bool
initPlayer p = E.withPlayerMeta p $ do
  g <- get
  initialCardsNo <- cfgHandCardsNo <$> ask
  let deckNo = fromEnum p
      deck = fmap DHandCard $ view gsAbilityDecks g `S.index` deckNo
  E.pipeToMeta $ yield $ ShowDeck deck $ ForChoice initialCardsNo
  timer <- lift $ setTimeout (Delay 60)
  r <- E.withTimer timer $ awaitList initialCardsNo
  case r of 
    Just list -> do
      let cards = map (\(PlayerChoice x) -> x) list
      -- should be atomic
      ok <- lift $ do
        g' <- get
        case drawAbilityCards p deckNo cards g' of
          Just newG -> put newG >> return True
          Nothing -> return False
      newG' <- get
      when ok $ E.pipeToMeta $ yield $ UpdateState $ getVisibleState newG' p
      return ok
    Nothing -> return False

updateState :: PId -> Act ()
updateState p = do
  g <- get
  E.sendToMeta p $ UpdateState $ getVisibleState g p

updateStates :: Act ()
updateStates = mapM_ updateState [Player1, Player2]

startGame :: Act Bool
startGame = do
  updateStates
  (ok1, ok2) <- paralBoth (initPlayer Player1) (initPlayer Player2)
  when (ok1 && ok2) $ updateStates 
  return $ ok1 && ok2

playGame :: Act ()
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
