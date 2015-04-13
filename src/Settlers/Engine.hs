module Settlers.Engine where

import qualified Engine as E
import Settlers.Core

type EnginePipe a b m = E.EnginePipe Settlers a b m
type EngineAction m = E.EngineAction Settlers m
