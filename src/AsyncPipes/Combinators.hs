module AsyncPipes.Combinators
( pfilter, pmap )
where

import AsyncPipes
import Control.Monad

pmap :: Monad m => (a -> b) -> Cond a b m ()
pmap f = waitAnd $ \x -> yield (f x) >> pmap f

pfilter :: Monad m => (a -> Bool) -> Cond a a m ()
pfilter f = waitAnd $ \x -> when (f x) (yield x) >> pfilter f
