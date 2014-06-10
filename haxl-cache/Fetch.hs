{-# LANGUAGE ExistentialQuantification, GADTs, StandaloneDeriving #-}
module Fetch where

import Types
import MockData
import DataCache
import Prelude hiding (lookup)

import Data.IORef
import Control.Applicative
import Data.Traversable
import Data.Sequence as Seq
import Data.Monoid
import Data.Foldable (toList)

data Result a
  = Done a
  | Blocked (Seq BlockedRequest) (Fetch a)

-- <<Fetch
newtype Fetch a = Fetch {
  unFetch :: IORef DataCache -> IO (Result a) }
-- >>

instance Monad Fetch where
  return a = Fetch $ \ref -> return (Done a)

  Fetch m >>= k = Fetch $ \ref -> do
    r <- m ref
    case r of
      Done a    -> unFetch (k a) ref
      Blocked br c -> return (Blocked br (c >>= k))


-- <<BlockedRequest
data BlockedRequest =
  forall a . BlockedRequest (Request a)
                            (IORef (FetchStatus a))
-- >>

-- <<dataFetch
dataFetch :: Request a -> Fetch a
dataFetch req = Fetch $ \ref -> do
  cache <- readIORef ref
  case lookup req cache of
    Nothing -> do
      box <- newIORef NotFetched
      writeIORef ref (insert req box cache)
      let br = BlockedRequest req box
      return (Blocked (singleton br) (cont box))
    Just box -> do
      r <- readIORef box
      case r of
        FetchSuccess result -> return (Done result)
        NotFetched -> return (Blocked Seq.empty (cont box))
 where
  cont box = Fetch $ \ref -> do
    FetchSuccess a <- readIORef box
    return (Done a)
-- >>

instance Functor Fetch where
  fmap f h = do x <- h; return (f x)

-- <<Applicative
instance Applicative Fetch where
  pure = return

  Fetch f <*> Fetch x = Fetch $ \ref -> do
    f' <- f ref
    x' <- x ref
    case (f',x') of
      (Done g,        Done y       ) -> return (Done (g y))
      (Done g,        Blocked br c ) -> return (Blocked br (g <$> c))
      (Blocked br c,  Done y       ) -> return (Blocked br (c <*> return y))
      (Blocked br1 c, Blocked br2 d) -> return (Blocked br (c <*> d))
        where br = br1 <> br2
-- >>


-- <<runFetch
runFetch :: Fetch a -> IO a
runFetch h = do
  ref <- newIORef DataCache.empty
  runFetch' ref h

runFetch' :: IORef DataCache -> Fetch a -> IO a
runFetch' ref (Fetch h) = do
  r <- h ref
  case r of
    Done a -> return a
    Blocked br cont -> do
      fetch (toList br)
      runFetch' ref cont
-- >>

fetch :: [BlockedRequest] -> IO ()
fetch bs = do
  putStrLn "==== fetch ===="
  mapM_ ppr bs
  where ppr (BlockedRequest r m) = do
          print r
          writeIORef m (FetchSuccess (requestVal r))

mapM :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
mapM = traverse

sequence :: (Applicative f, Traversable t) => t (f a) -> f (t a)
sequence = Data.Traversable.sequenceA
