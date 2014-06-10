module Blog where

import Types
import Fetch

import Control.Applicative
import Data.Time
import Data.List
import Data.Function
import Data.Ord
import qualified Data.Map as Map
import Data.Map (Map)
import Prelude hiding (mapM, sequence)

-- -----------------------------------------------------------------------------
-- IO

getPostIds :: Fetch [PostId]
getPostInfo :: PostId -> Fetch PostInfo
getPostContent :: PostId -> Fetch PostContent
getPostViews :: PostId -> Fetch Int

-- <<dataFetches
getPostIds     = dataFetch FetchPosts
getPostInfo    = dataFetch . FetchPostInfo
getPostContent = dataFetch . FetchPostContent
getPostViews   = dataFetch . FetchPostViews
-- >>

-- -----------------------------------------------------------------------------
-- Blog code

data Html = Html

-- <<blog
blog :: Fetch Html
blog = renderPage <$> leftPane <*> mainPane
-- >>

-- <<getAllPosts
getAllPostsInfo :: Fetch [PostInfo]
getAllPostsInfo = mapM getPostInfo =<< getPostIds
-- >>

-- <<getPostDetails
getPostDetails :: PostId
               -> Fetch (PostInfo, PostContent)
getPostDetails pid =
 (,) <$> getPostInfo pid <*> getPostContent pid
-- >>

-- <<leftPane
leftPane :: Fetch Html
leftPane = renderSidePane <$> popularPosts <*> topics
-- >>

-- <<topics
topics :: Fetch Html
topics = do
  posts <- getAllPostsInfo
  let topiccounts =
        Map.fromListWith (+)
          [ (postTopic p, 1) | p <- posts ]
  return $ renderTopics topiccounts
-- >>

-- <<popularPosts
popularPosts :: Fetch Html
popularPosts = do
  pids <- getPostIds
  views <- mapM getPostViews pids
  let ordered =
        take 5 $ map fst $
        sortBy (flip (comparing snd))
               (zip pids views)
  content <- mapM getPostDetails ordered
  return $ renderPostList content
-- >>

-- <<mainPane
mainPane :: Fetch Html
mainPane = do
  posts <- getAllPostsInfo
  let ordered =
        take 5 $
        sortBy (flip (comparing postDate)) posts
  content <- mapM (getPostContent . postId) ordered
  return $ renderPosts (zip ordered content)
-- >>

-- -----------------------------------------------------------------------------
-- Dummy rendering

renderPage :: Html -> Html -> Html
renderPage _ _ = Html

renderPosts :: [(PostInfo, PostContent)] -> Html
renderPosts _ = Html

renderSidePane :: Html -> Html -> Html
renderSidePane _ _ = Html

renderPostList :: [(PostInfo, PostContent)] -> Html
renderPostList _ = Html

renderTopics :: Map String Int -> Html
renderTopics _ = Html
