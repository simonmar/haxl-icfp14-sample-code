{-# LANGUAGE ExistentialQuantification, GADTs, StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Types where

import Data.Time
import Data.Typeable
import Data.Hashable

-- -----------------------------------------------------------------------------
-- Types

type Id = Int
type Date = UTCTime

newtype PostId = PostId Id
  deriving (Eq, Ord, Num, Hashable)

type PostContent = String

-- <<Types
data PostInfo = PostInfo
  { postId    :: PostId
  , postDate  :: Date
  , postTopic :: String
  }
-- >>

-- <<Request
data Request a where
  FetchPosts       :: Request [PostId]
  FetchPostInfo    :: PostId -> Request PostInfo
  FetchPostContent :: PostId -> Request PostContent
  FetchPostViews   :: PostId -> Request Int
-- >>

deriving instance Show PostId
deriving instance Show PostInfo
deriving instance Show (Request a)

deriving instance Eq (Request a)

instance Hashable (Request a) where
  hashWithSalt salt FetchPosts = hashWithSalt salt (0::Int)
  hashWithSalt salt (FetchPostInfo p) = hashWithSalt salt (1::Int, p)
  hashWithSalt salt (FetchPostContent p) = hashWithSalt salt (2::Int, p)
  hashWithSalt salt (FetchPostViews p) = hashWithSalt salt (3::Int, p)

-- <<FetchStatus
data FetchStatus a = NotFetched | FetchSuccess a
-- >>

