{-# LANGUAGE GeneralizedNewtypeDeriving, GADTs,
  MultiParamTypeClasses, KindSignatures, FlexibleInstances #-}
module EngineMonad where

import Data.Functor
import Control.Monad.Operational
import Control.Monad.State
import Control.Monad.Reader

import qualified Control.Concurrent.Timer as T
import Data.Int

class (MonadReader settings m, MonadState state m, MonadTimer m) => MonadEngine settings state m where

class Monad m => MonadTimer m where
  setTimeout :: Delay -> m Timer
  killTimer ::  Timer -> m ()

newtype EngineMonad settings state m r = 
  EM (StateT state (ReaderT settings (EngineT m)) r )
  deriving (Functor, Monad, Applicative, MonadState state, MonadIO, MonadReader settings)

type EngineT m = ProgramT (EngineInstr m) m

newtype Delay = Delay Int64

data Timer = Timer T.TimerIO Int

instance Eq Timer where
  Timer _ id1 == Timer _ id2 = id1 == id2

data EngineInstr (m :: * -> *) r where
  SetTimeout :: Delay -> EngineInstr m Timer
  KillTimer :: Timer -> EngineInstr m ()

instance Monad m => MonadTimer (EngineMonad settings state m) where
  setTimeout = EM . lift . lift . singleton . SetTimeout
  killTimer = EM . lift . lift . singleton . KillTimer

instance Monad m => MonadEngine settings state (EngineMonad settings state m)
