{-# LANGUAGE TypeFamilies, StandaloneDeriving, FlexibleInstances, FlexibleContexts #-}
module Main where

import Settlers.Core
import Settlers.Game
import Settlers.ConsoleGameInstances
import Settlers.Settings
import Engine
import EngineMonad

import Control.Monad
import Control.Monad.State

import System.IO
import Data.Maybe

import Pipes
import Pipes.Concurrent
import Pipes.PseudoParal
import qualified Pipes.Prelude as PP

import Game
import qualified ConsoleGame as CG

deriving instance Show (EngineIn Simplest)
deriving instance Show (EngineOut Simplest)

settlers :: MonadEngine Settlers m => EngineAction Settlers m ()
settlers = playGame

main :: IO ()
main = do
  (i,o,x,cln) <- mkGameRunner defaultSettings defaultState settlers 
  CG.spawnConsoleIO i o cln
  x
  return ()
