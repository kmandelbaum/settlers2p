{-# LANGUAGE FlexibleInstances #-}
module Settlers.ConsoleGameInstances where

import Settlers.Core
import Game
import ConsoleGame

instance ToString (DataToPlayer Settlers) where
  toString = show

instance FromString (DataFromPlayer Settlers) where
  fromString = PlayerChoice . read

instance (ConsoleGame Settlers)
