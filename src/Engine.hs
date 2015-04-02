{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, GADTs, DataKinds, AllowAmbiguousTypes #-}
module Engine where

import Game

import Control.Monad
import Control.Monad.State
import Control.Monad.Logger

import Data.Sequence
import Pipes
import Pipes.PseudoParal
import qualified Pipes.Prelude as PP
import qualified Data.Proxy as Proxy

data EngineIn g = FromPlayer (PlayerId g) (DataFromPlayer g)
data EngineOut g = ToPlayer (PlayerId g) (DataToPlayer g)

data EngineMeta = EngineMeta

type EnginePipe g i o = Pipe i o (EngineMonad g)

type EnginePipeMeta g i o = Pipe (Either EngineMeta i) (Either EngineMeta o) (EngineMonad g)

type EngineAction g = EnginePipeMeta g (EngineIn g) (EngineOut g)

type EngineMonad g = LoggingT (State (GameState g))

sendTo :: PlayerId g -> DataToPlayer g -> EngineAction g ()
sendTo p d = yield $ Right $ ToPlayer p d

filterPlayer :: Game g => PlayerId g -> EnginePipe g (EngineIn g) (DataFromPlayer g) r
filterPlayer p = forever $ do
  x <- await
  when (isPlayer p x) $ yield $ getD x
  where
    isPlayer p (FromPlayer p' _) = p == p'
    getD (FromPlayer _ d) = d

pipeToAction :: EnginePipe g i o r -> EnginePipeMeta g i o r
pipeToAction p = body >~ (p >-> PP.map Right)
  where body = do
          i <- await
          either (\x -> yield (Left x) >> body) (return) i
