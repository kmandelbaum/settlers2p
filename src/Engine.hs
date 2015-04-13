{-# LANGUAGE GeneralizedNewtypeDeriving, 
 TypeFamilies, GADTs, DataKinds, AllowAmbiguousTypes #-}
module Engine where

import Game
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

data EngineIn g = FromPlayer (PlayerId g) (DataFromPlayer g)
data EngineOut g = ToPlayer (PlayerId g) (DataToPlayer g)

data EngineMeta = Timeout Timer | EngineMeta

type EnginePipe g i o m = Pipe i o (EngineMonad g m)

type EnginePipeMeta g i o m = EnginePipe g (Either EngineMeta i) (Either EngineMeta o) m

type EngineAction g m = EnginePipeMeta g (EngineIn g) (EngineOut g) m

sendTo :: Monad m => PlayerId g -> DataToPlayer g -> EngineAction g m ()
sendTo p d = yield $ Right $ ToPlayer p d

filterPlayer :: (Monad m, Game g) => PlayerId g -> EnginePipe g (EngineIn g) (DataFromPlayer g) m r
filterPlayer p = forever $ do
  x <- await
  when (isPlayer p x) $ yield $ getD x
  where
    isPlayer p (FromPlayer p' _) = p == p'
    getD (FromPlayer _ d) = d

toPlayer :: (Monad m, Game g) => PlayerId g -> EnginePipe g (DataToPlayer g) (EngineOut g) m r
toPlayer = PP.map . ToPlayer

filterPlayerMeta :: (Monad m, Game g) => PlayerId g -> EnginePipeMeta g (EngineIn g) (DataFromPlayer g) m r
filterPlayerMeta = pipeToMeta . filterPlayer

toPlayerMeta :: (Monad m, Game g) => PlayerId g -> EnginePipeMeta g (DataToPlayer g) (EngineOut g) m r
toPlayerMeta = pipeToMeta . toPlayer

withPlayer :: 
  (Monad m, Game g) => 
  PlayerId g -> 
  EnginePipe g (DataFromPlayer g) (DataToPlayer g) m r ->
  EnginePipe g (EngineIn g) (EngineOut g) m r
withPlayer p pi = filterPlayer p >-> pi >-> toPlayer p

withPlayerMeta :: 
  (Monad m, Game g) => 
  PlayerId g -> 
  EnginePipeMeta g (DataFromPlayer g) (DataToPlayer g) m r ->
  EnginePipeMeta g (EngineIn g) (EngineOut g) m r
withPlayerMeta p pi = filterPlayerMeta p >-> pi >-> toPlayerMeta p

pipeToMeta :: Monad m => EnginePipe g i o m r -> EnginePipeMeta g i o m r
pipeToMeta p = body >~ (p >-> PP.map Right)
  where body = await >>= 
          either ((>> body) . yield . Left) return

withTimer :: Monad m => Timer -> EnginePipe g i o m r -> EnginePipeMeta g i o m (Maybe r)
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

runEngineT :: (MonadIO m) => Output (Either EngineMeta i) -> EngineT g m r -> m r
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

mkGameRunner :: (Applicative m, MonadIO m) => GameSettings g -> GameState g -> EnginePipeMeta g i o m () -> IO (Output i, Input o, m (), IO ())
mkGameRunner cfg g e = do
  (inChanIn@(Output ii), inChanOut, sealIn) <- liftIO $ spawn' unbounded
  (outChanIn, outChanOut, sealOut) <- liftIO $ spawn' unbounded
  let z = fmap fst $ runEngineT inChanIn $ runStateT (runReaderT ef cfg) g
      EM ef = (runEffect (((fromInput inChanOut >-> e) `for` body) >-> toOutput outChanIn)) 
  return (Output (ii . Right), outChanOut, z, atomically(sealIn >> sealOut))
  where body (Right x) = yield x
        body _ = return ()

mkDelay :: Delay -> Suspend.Delay
mkDelay (Delay d) = Suspend.sDelay d
