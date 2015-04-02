module Settlers.Engine where

import qualified Engine as E
import Settlers.Core

type EnginePipe a b = E.EnginePipe Settlers a b
type EngineAction = E.EngineAction Settlers
