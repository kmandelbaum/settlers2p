{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, GADTs, DataKinds, AllowAmbiguousTypes, DeriveFunctor,
  MultiParamTypeClasses #-}
module EngineMonad where

import Data.Functor
import Control.Monad
import Control.Applicative
import Control.Monad.Operational
import Control.Monad.State
import Control.Monad.Reader

import Game
import qualified Data.Map as M
import qualified Control.Concurrent.Timer as T
import Data.Int

newtype EngineMonad g m r = 
  EM (StateT (GameState g) (ReaderT (GameSettings g) (EngineT g m)) r )
  deriving (Functor, Monad, Applicative, MonadState (GameState g), MonadIO, MonadReader(GameSettings g))

type EngineT g m = ProgramT (EngineInstr g m) m

newtype Delay = Delay Int64

data Timer = Timer T.TimerIO Int

instance Eq Timer where
  Timer _ id1 == Timer _ id2 = id1 == id2

data EngineInstr g (m :: * -> *) r where
  SetTimeout :: Delay -> EngineInstr g m Timer
  KillTimer :: Timer -> EngineInstr g m ()

setTimeout :: Monad m => Delay -> EngineMonad g m Timer
setTimeout = EM . lift . lift . singleton . SetTimeout

killTimer :: Monad m => Timer -> EngineMonad g m ()
killTimer = EM . lift . lift . singleton . KillTimer
