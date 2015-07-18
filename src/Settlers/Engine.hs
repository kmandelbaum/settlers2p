{-# LANGUAGE FlexibleContexts, FlexibleInstances, ConstraintKinds #-}
module Settlers.Engine where

import qualified Engine as E
import qualified EngineMonad as E
import Settlers.Core

type EngineAction m = E.EngineAction Settlers m

type EnginePipe m = E.EnginePipe Settlers m

type MonadEngine m = E.MonadEngine Settlers m
