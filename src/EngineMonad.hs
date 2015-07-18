{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, GADTs, DataKinds, AllowAmbiguousTypes, DeriveFunctor,
  MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}
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

class (MonadReader (GameSettings g) m, MonadState (GameState g) m, MonadTimer m) => MonadEngine g m where

class Monad m => MonadTimer m where
  setTimeout :: Delay -> m Timer
  killTimer ::  Timer -> m ()

newtype EngineMonad g m r = 
  EM (StateT (GameState g) (ReaderT (GameSettings g) (EngineT m)) r )
  deriving (Functor, Monad, Applicative, MonadState (GameState g), MonadIO, MonadReader(GameSettings g))

type EngineT m = ProgramT (EngineInstr m) m

newtype Delay = Delay Int64

data Timer = Timer T.TimerIO Int

instance Eq Timer where
  Timer _ id1 == Timer _ id2 = id1 == id2

data EngineInstr (m :: * -> *) r where
  SetTimeout :: Delay -> EngineInstr m Timer
  KillTimer :: Timer -> EngineInstr m ()

instance Monad m => MonadTimer (EngineMonad g m) where
  setTimeout = EM . lift . lift . singleton . SetTimeout
  killTimer = EM . lift . lift . singleton . KillTimer

instance Monad m => MonadEngine g (EngineMonad g m)
