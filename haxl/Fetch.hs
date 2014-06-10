{-# LANGUAGE ExistentialQuantification, GADTs, StandaloneDeriving #-}
module Fetch where

import Types
import MockData

import Data.IORef
import Control.Applicative
import Data.Traversable
import Data.Sequence
import Data.Monoid
import Data.Foldable (toList)

-- <<Monad
data Result a
  = Done a
  | Blocked (Seq BlockedRequest) (Fetch a)

newtype Fetch a = Fetch { unFetch :: IO (Result a) }

instance Monad Fetch where
  return a = Fetch $ return (Done a)

  Fetch m >>= k = Fetch $ do
    r <- m
    case r of
      Done a       -> unFetch (k a)
      Blocked br c -> return (Blocked br (c >>= k))
-- >>


-- <<BlockedRequest
data BlockedRequest =
  forall a . BlockedRequest (Request a)
                       (IORef (FetchStatus a))
-- >>

-- <<dataFetch
dataFetch :: Request a -> Fetch a
dataFetch request = Fetch $ do
  box <- newIORef NotFetched             -- (1)
  let br = BlockedRequest request box   -- (2)
  let cont = Fetch $ do                 -- (3)
        FetchSuccess a <- readIORef box  -- (4)
        return (Done a)                 -- (5)
  return (Blocked (singleton br) cont)  -- (6)
-- >>

instance Functor Fetch where
  fmap f h = do x <- h; return (f x)

-- <<Applicative
instance Applicative Fetch where
  pure = return

  Fetch f <*> Fetch x = Fetch $ do
    f' <- f
    x' <- x
    case (f',x') of
      (Done g,        Done y       ) -> return (Done (g y))
      (Done g,        Blocked br c ) -> return (Blocked br (g <$> c))
      (Blocked br c,  Done y       ) -> return (Blocked br (c <*> return y))
      (Blocked br1 c, Blocked br2 d) -> return (Blocked (br1 <> br2) (c <*> d))
-- >>

-- <<runFetch
runFetch :: Fetch a -> IO a
runFetch (Fetch h) = do
  r <- h
  case r of
    Done a -> return a
    Blocked br cont -> do
      fetch (toList br)
      runFetch cont
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
