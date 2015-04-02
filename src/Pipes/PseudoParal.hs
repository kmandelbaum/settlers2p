module Pipes.PseudoParal where

import Pipes
import qualified Pipes.Prelude as PP
import Data.Either
import Data.Functor
import Pipes.Internal
import Pipes.Core

paralAny :: Monad m => Pipe i o m r -> Pipe i o m r -> Pipe i o m r
paralAny p1 p2 = shuntL p1 >-> shuntR p2

shuntL :: Monad m => Pipe i o m r -> Pipe i (Either i o) m r
shuntL p = body >~ (p >-> PP.map Right)
  where 
    body = do
      i <- await
      yield $ Left i
      return i

shuntR :: Monad m => Pipe i o m r -> Pipe (Either i o) o m r
shuntR p = body >~ p
  where 
    body = do
      i <- await
      case i of
        Left x -> return x
        Right y -> yield y >> body

paralBoth :: Monad m => Pipe i o m r0 -> Pipe i o m r1 -> Pipe i o m (r0, r1)
paralBoth = go
  where 
    go p0 p1 = case p0 of
      Request () f0 -> goRequest f0 p1
      Respond vo f0 -> respond vo >> go (f0 ()) p1
      M m -> do
        p <- lift m
        go p p1
      Pure r0 -> (,) r0 <$> p1
    goRequest f0 p1 = case p1 of
      Request () f1 -> do
        i <- request ()
        go (f0 i) (f1 i)
      Respond vo f1 -> respond vo >> goRequest f0 (f1 ())
      M m -> do
        p <- lift m
        goRequest f0 p
      Pure r1 -> flip (,) r1 <$> (request () >>= f0)
