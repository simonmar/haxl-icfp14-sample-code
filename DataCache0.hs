{-# LANGUAGE RankNTypes #-}
module DataCache0 where

import Types
import Unsafe.Coerce

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
type Map k v = HashMap k v

-- <<DataCache
newtype DataCache =
  DataCache (forall a . Map (Request a) a)
-- >>

-- <<lookup
lookup :: Request a -> DataCache -> Maybe a
lookup key (DataCache m) = Map.lookup key m
-- >>

-- <<insert
insert :: Request a -> a -> DataCache -> DataCache
insert key val (DataCache m) =
  DataCache $ unsafeCoerce (Map.insert key val m)
-- >>
