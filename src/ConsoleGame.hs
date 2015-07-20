{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses #-}
module ConsoleGame where

import Engine
import Pipes.Concurrent
import Control.Concurrent
import Data.Maybe
import System.IO

class FromString a where
  fromString :: String -> a

class ToString a where
  toString :: a -> String

class (FromString dataIn, ToString dataOut, Enum p) => ConsoleGame p dataIn dataOut

spawnConsoleIO ::
  ConsoleGame p dataIn dataOut =>
  Output (EngineIn p dataIn) ->
  Input (EngineOut p dataOut) ->
  IO () ->
  IO ()
spawnConsoleIO o i cln = do
  forkIO reader
  forkIO writer
  return ()
  where
    reader = do
      eof <- isEOF
      if not eof then do
        str <- getLine
        let playerId = toEnum $ read $ head $ words str
            strToSend = unwords $ tail $ words str
        continue <- atomically $ send o (FromPlayer playerId (fromString strToSend))
        if continue then reader else cln
      else cln
      
    writer = do
      z <- atomically $ recv i
      if (isJust z) then putStrLn (engineOutToString $ fromJust z) >> writer else cln
    engineOutToString e =
      case e of
        ToPlayer pid tp -> show (fromEnum pid) ++ " > " ++ toString tp
        _ -> "Engine outputted something"
