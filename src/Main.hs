module Main where

import AsyncPipes
import AsyncPipes.Combinators

import Settlers
import Settlers.GameInstances
import Engine

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TMChan

import Data.Maybe

import System.IO


main :: IO ()
main = undefined

mkSinkChan :: Show s => IO (TMChan s)
mkSinkChan = do
  c <- newTMChanIO
  let loop = do
        x <- atomically $ readTMChan c
        when (isJust x) $ do
          print $ "Sink: " ++ show (fromJust x)
          loop
  forkIO loop
  return c

mkSourceChan :: IO (TMChan String)
mkSourceChan = do
  c <- newTMChanIO
  let loop = do
        x <- isEOF
        if not x then do
          s <- getLine
          isClosed <- atomically $ do
            isClosed <- isClosedTMChan c
            when (not isClosed) $ writeTMChan c s
            return isClosed
          when (not isClosed) loop
        else
          atomically $ closeTMChan c
  forkIO loop
  return c
