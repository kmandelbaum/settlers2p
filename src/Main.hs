{-# LANGUAGE TypeFamilies, StandaloneDeriving, FlexibleInstances #-}
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

data Simplest

instance Game Simplest where
  data GameState Simplest = St Int deriving (Eq, Show)
  data GameSettings Simplest = Gs Int deriving (Eq, Show)
  data PlayerId Simplest = PID Int deriving (Eq, Show)
  data VisibleState Simplest = Vs Int deriving (Eq, Show)
  data DataToPlayer Simplest = DToPlayer String deriving (Eq, Show)
  data DataFromPlayer Simplest = DFromPlayer Int deriving (Eq, Show)

eng :: Monad m => EnginePipe 
  Simplest (DataFromPlayer Simplest) (DataToPlayer Simplest) m r
eng = forever $ do
  (DFromPlayer d) <- await
  (St s) <- get
  put (St (s + d))
  yield (DToPlayer $ show (s + d))

turn :: Monad m => EnginePipe 
  Simplest (DataFromPlayer Simplest) (DataToPlayer Simplest) m ()
turn = do
  (DFromPlayer i) <- await
  (DFromPlayer j) <- await
  (St s) <- get
  put (St (s + i + j))
  yield $ DToPlayer $ show (s + i + j)

eng' :: Monad m => EngineAction Simplest m ()
eng' = forever $ do
  t <- lift $ setTimeout (Delay 2)
  res <- withTimer t (withPlayer (PID 0) turn)
  when (isNothing res) $ sendTo (PID 0) (DToPlayer "timeout")

simplest ::Monad m => EngineAction Simplest m ()
simplest = do
  withPlayerMeta (PID 0) (pipeToMeta eng)

spawnIO ::
  Output (EngineIn Simplest) ->
  Input (EngineOut Simplest) ->
  IO () ->
  IO ()
spawnIO o i cln = do
  forkIO reader
  forkIO writer
  return ()
  where
    reader = do
      eof <- isEOF
      if not eof then do
        str <- getLine
        continue <- atomically $ send o (FromPlayer (PID 0) (DFromPlayer (read str)))
        if continue then reader else cln
      else cln
      
    writer = do
      z <- atomically $ recv i
      if (isJust z) then print (fromJust z) >> writer else cln

settlers = playGame

main :: IO ()
main = do
  (i,o,x,cln) <- mkGameRunner defaultSettings defaultState settlers 
  CG.spawnConsoleIO i o cln
  x
  return ()
  --(i,o,x,cln) <- mkGameRunner (Gs 0) (St 0) eng'
  --spawnIO i o cln
  --x
  --return ()
