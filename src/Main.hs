module Main where


import Settlers.Core
import Settlers.Game
import Engine

import Control.Monad

import Data.Maybe

import System.IO
import Pipes
import Pipes.PseudoParal
import qualified Pipes.Prelude as PP

pipe1 :: (Monad m, Num i, Show i) => Pipe i String m ()
pipe1 = forever $ do
  i <- await
  yield $ "1: " ++ show (i * 3)

pipe2 :: (Monad m, Num i, Show i) => Pipe i String m ()
pipe2 = forever $ do
  i <- await
  j <- await
  yield $ "2: " ++ show (i * j)

src :: Producer' Int IO r
src = PP.readLn >> undefined

sink :: Show a => Consumer' a IO r
sink = PP.print

main :: IO ()
main = do
  runEffect $ src >-> paralAny (pipe1 >-> PP.take 5) (pipe2 >-> PP.take 2) >-> sink
  return ()

