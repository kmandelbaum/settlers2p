module Maps (Id, IdMap, idMapFromList) where

-- Consider doing the whole thing with an array

import qualified Data.Map as M

type GenericId = Int

newtype Id a = Id { _id :: GenericId } deriving (Eq, Ord, Show)

type IdMap a = M.Map (Id a) a

idMapFromList = M.fromList . zip (map Id [0..])
