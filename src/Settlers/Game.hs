module Settlers.Game where

import Settlers.Engine
import Settlers.Core

import Control.Applicative
import Control.Monad

import Pipes.PseudoParal
import Pipes
import Pipes.Prelude as PP

import qualified Engine as E
import Data.Proxy as Proxy
import qualified Game as G


playTurn :: EngineAction Bool
playTurn = return False


initPlayer :: PlayerId -> EngineAction Bool
initPlayer p = undefined --p2a (E.filterPlayer p >-> t)
  where
    interaction = undefined
startGame :: EngineAction Bool
startGame = do
  (ok1, ok2) <- paralBoth (initPlayer Player1) (initPlayer Player2)
  return $ ok1 && ok2

playGame :: EngineAction ()
playGame = do
  isOk <- startGame
  when isOk $ while playTurn

--Utilities
while :: Monad m => m Bool -> m ()
while cond = do
  continue <- cond
  when continue $ while cond

