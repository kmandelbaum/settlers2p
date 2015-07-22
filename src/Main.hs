{-# LANGUAGE TypeFamilies, StandaloneDeriving, FlexibleInstances, FlexibleContexts #-}
module Main where

import Settlers.Settings
import Settlers.Game
import Settlers.ConsoleGameInstances

import Control.Monad
import Control.Monad.State

import System.IO
import Data.Maybe

import Engine

import qualified ConsoleGame as CG

main :: IO ()
main = do
  (i,o,x,cln) <- mkGameRunner defaultSettings defaultState playGame 
  CG.spawnConsoleIO i o cln
  x
  return ()
