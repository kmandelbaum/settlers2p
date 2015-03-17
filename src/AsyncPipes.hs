{-# LANGUAGE GADTs, ScopedTypeVariables #-} 
module AsyncPipes 
(
  Async,
  Void,
  Cond,
  wait,
  iwait,
  waitAnd,
  waitAndOr,
  yield,
  fuse,
  run,
  runAsync,
  spawn,
  paral
)
where

import Control.Applicative
import Control.Monad
import Control.Monad.Operational
import Control.Monad.Trans
import Control.Monad.State

import Control.Concurrent( threadDelay, forkIO, killThread, ThreadId )
import Control.Concurrent.STM
import Control.Concurrent.STM.TMChan

import Data.Maybe

import Text.Read( readMaybe )

-- Data types
data Void

type Async a = (TMChan a, [ThreadId])

data CondI i o m r where
  Wait :: Monad m => CondI i o m (Maybe i)
  Yield :: Monad m => o -> CondI i o m ()
  IWait :: MonadIO m => Async a -> CondI i o m (Either a (Maybe i))
  Spawn :: MonadIO m => IO a -> CondI i o m (Async a)

type Cond i o m r = ProgramT (CondI i o m) m r


-- Functions in the `Cond i o m` monad

yield :: Monad m => o -> Cond i o m ()
yield = singleton . Yield

wait :: Monad m => Cond i o m (Maybe i)
wait = singleton Wait

waitAnd :: Monad m => (i -> Cond i o m ()) -> Cond i o m ()
waitAnd cont = wait >>= ifF isJust (cont . fromJust) (const $ return ())

waitAndOr :: Monad m => (i -> Cond i o m r) -> Cond i o m r -> Cond i o m r
waitAndOr cont' cont'' =  wait >>= ifF isJust (cont' . fromJust) (const cont'')

iwait :: MonadIO m => Async a -> Cond i o m (Either a (Maybe i))
iwait = singleton . IWait

spawn :: MonadIO m => IO a -> Cond i o m (Async a)
spawn = singleton . Spawn

fuse :: Monad m => Cond i x m () -> Cond x o m r -> Cond i o m r
fuse p1 p2 = goRight p1 p2
  where
    goRight :: Monad m => Cond i x m () -> Cond x o m r -> Cond i o m r
    goRight p1 p2 = do
      p2view <- lift $ viewT p2
      case p2view of
           Wait :>>= g      -> goLeft p1 g
           Yield vo :>>= g  -> yield vo >> goRight p1 (g ())
           Return vr        -> return vr
           IWait a :>>= g   -> goIWaitLeft a p1 g
           Spawn act :>>= g -> spawn act >>= goRight p1 . g

    goLeft :: Monad m => Cond i x m () -> (Maybe x -> Cond x o m r) -> Cond i o m r
    goLeft p1 fp2 = do
      p1view <- lift $ viewT p1
      case p1view of
           Wait :>>= g      -> wait >>= \vi -> goLeft (g vi) fp2
           Yield vx :>>= g  -> goRight (g ()) (fp2 $ Just vx)
           Return ()        -> goRight (return ()) (fp2 Nothing)
           IWait a :>>= g   -> iwait a >>= \vi -> goLeft (g vi) fp2
           Spawn act :>>= g -> spawn act >>= \a -> goLeft (g a) fp2

    goIWaitLeft :: MonadIO m => Async ta -> Cond i x m () -> (Either ta (Maybe x) -> Cond x o m r) -> Cond i o m r
    goIWaitLeft a p1 fp2 = do
      p1view <- lift $ viewT p1
      case p1view of
           Wait :>>= g -> do
             vvi <- iwait a
             case vvi of
                  Left va ->  goRight (wait >>= g) (fp2 $ Left va)
                  Right vi -> goIWaitLeft a (g vi) fp2
           Yield vx :>>= g -> liftIO (closeAsync a) >> goRight (g ()) (fp2 $ Right $ Just vx)
           Return () -> liftIO (closeAsync a) >> goRight (return ()) (fp2 $ Right Nothing)
           IWait la :>>= g -> do
             eitherAsync <- liftIO $ mergeAsyncEither la a True
             vai <- iwait eitherAsync
             liftIO $ closeAsync eitherAsync
             case vai of
                  Left (Left vla) -> liftIO (closeAsync la) >> goIWaitLeft a (g (Left vla)) fp2
                  Left (Right va) -> liftIO (closeAsync a) >> goRight (iwait la >>= g) (fp2 (Left va))
                  Right vi        -> liftIO (closeAsync la) >> goIWaitLeft a (g (Right vi)) fp2
           Spawn act :>>= g      -> spawn act >>= \a' -> goIWaitLeft a (g a') fp2

-- Executing async pipes

run :: Monad m => Cond Void Void m r -> m r
run p = do
  pview <- viewT p
  case pview of
      Wait :>>= g       -> undefined
      Yield v :>>= g    -> undefined
      IWait a :>>= g    -> undefined
      Return vr         -> return vr

runAsync :: forall i o m r . MonadIO m => TMChan i -> TMChan o -> Cond i o m r -> m r
runAsync iChan oChan cond = do
  pview <- viewT cond
  case pview of
       Wait :>>= g      -> (liftTM $ readTMChan iChan) >>= rec . g
       Yield vo :>>= g  -> (liftTM $ writeTMChan oChan vo) >>= rec . g
       Return vr        -> liftTM (closeTMChan iChan) >> liftTM (closeTMChan oChan) >> return vr
       IWait a :>>= g   -> do
         vvi <- liftIO $ do
           (tmpChan, threads) <- mergeAsyncWith True False (Left . fromJust) Right a (iChan,[]) True
           vvi' <- atomically $ readTMChan tmpChan
           closeAsync a
           mapM_ killThread threads
           return vvi'
         if isJust vvi then (rec $ g $ fromJust vvi) 
                       else error "Nothing came out of pipe"

       Spawn act :>>= g -> do
         a <- liftIO $ do
           c <- atomically newTMChan
           tid <- forkIO $ act >>= atomically . writeTMChan c
           return (c,[tid])
         rec $ g a
  where
    rec :: Cond i o m r -> m r
    rec = runAsync iChan oChan


-- Internal: async manipulation

mergeAsyncWith :: Bool -> Bool -> (Maybe a -> c) -> (Maybe b -> c) -> Async a -> Async b -> Bool -> IO (Async c)
mergeAsyncWith checka checkb fa fb (a,ta) (b,tb) closeOnEither = do

  tmpChan <- newTMChanIO

  newTa <- forkIO $ do
    va <- atomically $ peekTMChan a
    if (not checka || isJust va) then atomically $ do
      isClosed <- isClosedTMChan tmpChan
      when (not isClosed) $ do
        readTMChan a
        writeTMChan tmpChan (fa va)
        closeTMChan tmpChan
    else when closeOnEither $ atomically $ closeTMChan tmpChan

  newTb <- forkIO $ do
    vb <- atomically $ peekTMChan b
    if (not checkb || isJust vb) then atomically $ do
      isClosed <- isClosedTMChan tmpChan
      when (not isClosed) $ do
        readTMChan b
        writeTMChan tmpChan (fb vb)
        closeTMChan tmpChan 
    else when closeOnEither $ atomically $ closeTMChan tmpChan

  return (tmpChan, [newTa, newTb])

mergeAsyncEither :: Async a -> Async b -> Bool -> IO (Async (Either a b))
mergeAsyncEither = mergeAsyncWith True True (Left . fromJust) (Right . fromJust)

closeAsync :: Async a -> IO ()
closeAsync (chan, threads) = do 
  atomically $ closeTMChan chan
  mapM_ killThread threads

paral :: forall i o m r1 r2 . Monad m => Cond i o m r1 -> Cond i o m r2 -> Cond i o m (r1,r2)
paral up down = goUp up down
  where 
    goUp :: Cond i o m r1 -> Cond i o m r2 -> Cond i o m (r1,r2)
    goUp p1 p2 = do
      p1view <- lift $ viewT p1
      case p1view of
           Wait :>>= g1 -> goDown g1 p2
           Yield vo :>>= g1 -> yield vo >> goUp (g1 ()) p2
           Return vr1 -> do
             vr2 <- p2
             return (vr1, vr2)
           IWait a :>>= g1 -> goDownAsync a g1 p2
           Spawn act :>>= g1 -> spawn act >>= \a -> goUp (g1 a) p2
    goDown :: (Maybe i -> Cond i o m r1) -> Cond i o m r2 -> Cond i o m (r1,r2)
    goDown g1 p2 = do
      p2view <- lift $ viewT p2
      case p2view of
           Wait :>>= g2 -> wait >>= \vi -> goUp (g1 vi) (g2 vi)
           Yield vo :>>= g2 -> yield vo >> goDown g1 (g2 ())
           Return vr2 -> do
             vr1 <- wait >>= g1
             return (vr1, vr2)
           IWait a :>>= g2 -> do
             vvi <- iwait a
             case vvi of
                  Left _ -> goDown g1 (g2 vvi)
                  Right vi -> goUp (g1 vi) (g2 vvi)
           Spawn act :>>= g2 -> spawn act >>= \a -> goDown g1 (g2 a)
    goDownAsync :: MonadIO m => Async a -> (Either a (Maybe i) -> Cond i o m r1) -> Cond i o m r2 -> Cond i o m (r1,r2)
    goDownAsync a g1 p2 = do
      p2view <- lift $ viewT p2
      case p2view of
           Wait :>>= g2 -> do
             vvi <- iwait a
             case vvi of
                  Left _ -> goUp (g1 vvi) (wait >>= g2)
                  Right vi -> goUp (g1 vvi) (g2 vi)
           Yield vo :>>= g2 -> yield vo >> goDownAsync a g1 (g2 ())
           Return vr2 -> do
             vr1 <- iwait a >>= g1
             return (vr1, vr2)
           IWait a' :>>= g2 -> do
             aa' <- liftIO $ mergeAsyncEither a a' True
             vvi <- iwait aa'
             case vvi of
                  Right vi -> goUp (g1 (Right vi)) (g2 (Right vi))
                  Left (Left va) -> goUp (g1 (Left va)) (iwait a' >>= g2)
                  Left (Right va') -> goDownAsync a g1 (g2 (Left va'))
           Spawn act :>>= g2 -> spawn act >>= \ac -> goDownAsync a g1 (g2 ac)

-- Internal: Utilities

liftTM :: (MonadIO m) => STM a -> m a
liftTM = liftIO . atomically

if' :: Bool -> b -> b -> b
if' True x _ = x
if' False _ y = y

ifF :: (a -> Bool) -> (a -> b) -> (a -> b) -> a -> b
ifF = liftA3 if'
