{-# LANGUAGE FlexibleContexts, FlexibleInstances, ConstraintKinds, Rank2Types #-}
module Settlers.Engine where

import qualified Engine as E
import qualified EngineMonad as E
import Settlers.Core

type EngineAction m = E.EngineAction PlayerId DataFromPlayer DataToPlayer m

type EnginePipe m = E.EnginePipe PlayerId DataFromPlayer DataToPlayer m

type MonadEngine m = E.MonadEngine GameSettings GameState m

type Act a = forall m . MonadEngine m => EngineAction m a
