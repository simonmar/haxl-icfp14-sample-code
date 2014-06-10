module Test where

import Control.Applicative

-- <<Fetch
data Fetch a
  = Done a
  | Blocked (Fetch a)
-- >>

instance Functor Fetch where
  fmap f m = do x <- m; return (f x)

-- <<Monad
instance Monad Fetch where
  return = Done

  Done a    >>= k = k a
  Blocked c >>= k = Blocked (c >>= k)
-- >>

-- <<Applicative
instance Applicative Fetch where
  pure = return

  Done f    <*> x = f <$> x
  Blocked c <*> x = Blocked (c <*> x)
-- >>
