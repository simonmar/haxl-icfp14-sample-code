{-# LANGUAGE ExistentialQuantification, GADTs, StandaloneDeriving #-}
module MockData (requestVal) where

import Types

import Data.Time.Clock
import Data.Time.Calendar

requestVal :: Request a -> a
requestVal r = case r of
  FetchPosts                  -> map PostId postids
  FetchPostInfo (PostId n)    -> postinfos !! n
  FetchPostContent (PostId n) -> postcontent !! n
  FetchPostViews (PostId n)   -> postviews !! n
 where
  postids = [0..10] :: [Int]
  postinfos = map mkExamplePostInfo postids
  postcontent = [ "Post " ++ show n | n <- postids ]
  postviews = [ x `mod` 53 | x <- [ p, p + p .. ] ]
    where p = 10000001 :: Int

mkExamplePostInfo :: Int -> PostInfo
mkExamplePostInfo p = PostInfo
  { postId = PostId p
  , postDate = UTCTime (ModifiedJulianDay (fromIntegral p)) 0
  , postTopic = "Topic " ++ show (p `mod` 3)
  }

