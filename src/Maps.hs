module Maps (Id, IdMap) where

-- Consider doing the whole thing with an array

import qualified Data.Map as M

type GenericId = Int

newtype Id a = Id { _id :: GenericId } deriving (Eq, Ord)

type IdMap a = M.Map (Id a) a
