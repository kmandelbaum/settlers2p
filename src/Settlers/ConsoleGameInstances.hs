{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Settlers.ConsoleGameInstances where

import Settlers.Core
import ConsoleGame

instance ToString DataToPlayer where
  toString = show

instance FromString DataFromPlayer where
  fromString = PlayerChoice . read

instance ConsoleGame PlayerId DataFromPlayer DataToPlayer
