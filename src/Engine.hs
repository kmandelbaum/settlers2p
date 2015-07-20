{-# LANGUAGE GeneralizedNewtypeDeriving, 
 TypeFamilies, GADTs, DataKinds, FlexibleContexts #-}
module Engine where

import EngineMonad

import Control.Applicative
import Control.Monad
import Control.Monad.Operational
import Control.Monad.State
import Control.Monad.Reader

import qualified Control.Concurrent.Timer as T
import qualified Control.Concurrent.Suspend as Suspend

import Pipes
import Pipes.Concurrent
import Pipes.PseudoParal
import qualified Pipes.Prelude as PP

import qualified Data.Map as M

data EngineIn p dataIn = FromPlayer p dataIn
data EngineOut p dataOut = ToPlayer p dataOut

data EngineMeta = Timeout Timer | EngineMeta

type EnginePipeMeta i o m = Pipe (Either EngineMeta i) (Either EngineMeta o) m

type EnginePipe p dataIn dataOut m = Pipe (EngineIn p dataIn) (EngineOut p dataOut) m

type EngineAction p dataIn dataOut m = EnginePipeMeta (EngineIn p dataIn) (EngineOut p dataOut) m

sendTo :: Monad m => p -> dataOut -> EnginePipe p dataIn dataOut m ()
sendTo p d = yield $ ToPlayer p d

sendToMeta :: Monad m => p -> dataOut -> EngineAction p dataIn dataOut m ()
sendToMeta p d = pipeToMeta $ sendTo p d

filterPlayer :: (Monad m, Eq p) => p -> Pipe (EngineIn p dataIn) dataIn m r
filterPlayer p = forever $ do
  x <- await
  when (isPlayer p x) $ yield $ getD x
  where
    isPlayer p (FromPlayer p' _) = p == p'
    getD (FromPlayer _ d) = d

toPlayer :: Monad m => p -> Pipe dataOut (EngineOut p dataOut) m r
toPlayer = PP.map . ToPlayer

filterPlayerMeta :: (Monad m, Eq p) => p -> EnginePipeMeta (EngineIn p dataIn) dataIn m r
filterPlayerMeta = pipeToMeta . filterPlayer

toPlayerMeta :: Monad m => p -> EnginePipeMeta dataOut (EngineOut p dataOut) m r
toPlayerMeta = pipeToMeta . toPlayer

withPlayer :: 
  (Monad m, Eq p) => 
  p -> 
  Pipe dataIn dataOut m r ->
  Pipe (EngineIn p dataIn) (EngineOut p dataOut) m r
withPlayer p pi = filterPlayer p >-> pi >-> toPlayer p

withPlayerMeta :: 
  (Monad m, Eq p) => 
  p -> 
  EnginePipeMeta dataIn dataOut m r ->
  EnginePipeMeta (EngineIn p dataIn) (EngineOut p dataOut) m r
withPlayerMeta p pi = filterPlayerMeta p >-> pi >-> toPlayerMeta p

pipeToMeta :: Monad m => Pipe i o m r -> EnginePipeMeta i o m r
pipeToMeta p = body >~ (p >-> PP.map Right)
  where body = await >>= 
          either ((>> body) . yield . Left) return

withTimer :: MonadTimer m => Timer -> Pipe i o m r -> EnginePipeMeta i o m (Maybe r)
withTimer t p = guard >-> do
  res <- pipeToMeta p
  lift $ killTimer t
  return $ Just res
  where 
    guard = do
      x <- await
      case x of
        Left (Timeout t') -> 
          if t' == t then return Nothing 
                     else yield x >> guard
        Right d -> yield (Right d) >> guard

runEngineT :: MonadIO m => Output (Either EngineMeta i) -> EngineT m r -> m r
runEngineT i eng = go eng (0 :: Int)
  where 
    go e timerId = do
      p <- viewT e
      case p of
        SetTimeout d :>>= f -> do
          t <- liftIO T.newTimer
          let timer = Timer t timerId
          let fireTimeout = void $ atomically $ send i $ Left $ Timeout timer
          liftIO $ T.oneShotStart t fireTimeout (mkDelay d)
          go (f timer) (timerId + 1)
        KillTimer (Timer t _) :>>= f -> do 
          liftIO $ T.stopTimer t
          go (f ()) timerId
        Return r -> return r

mkGameRunner :: MonadIO m => 
                gsettings ->
                gstate ->
                EnginePipeMeta i o (EngineMonad gsettings gstate m) () ->
                IO (Output i, Input o, m (), IO ())
mkGameRunner cfg g e = do
  (inChanIn@(Output ii), inChanOut, sealIn) <- liftIO $ spawn' unbounded
  (outChanIn, outChanOut, sealOut) <- liftIO $ spawn' unbounded
  let z = runEngineT inChanIn $ runReaderT (evalStateT ef g) cfg
      EM ef = runEffect $ ((fromInput inChanOut >-> e) `for` body) >-> toOutput outChanIn 
  return (Output (ii . Right), outChanOut, z, atomically(sealIn >> sealOut))
  where body (Right x) = yield x
        body _ = return ()

mkDelay :: Delay -> Suspend.Delay
mkDelay (Delay d) = Suspend.sDelay d
