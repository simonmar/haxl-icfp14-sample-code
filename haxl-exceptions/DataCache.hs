{-# LANGUAGE RankNTypes #-}
module DataCache where

import Types
import Unsafe.Coerce
import Data.IORef

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
type Map k v = HashMap k v

-- <<DataCache
newtype DataCache =
  DataCache (forall a . Map (Request a) (IORef (FetchStatus a)))
-- >>

empty :: DataCache
empty = DataCache Map.empty

-- <<lookup
lookup :: Request a -> DataCache -> Maybe (IORef (FetchStatus a))
lookup key (DataCache cache) =
  Map.lookup key cache
-- >>

-- <<insert
insert :: Request a -> IORef (FetchStatus a) -> DataCache -> DataCache
insert key val (DataCache cache) =
  DataCache
    (unsafeCoerce
       (Map.insert key val cache))
-- >>
