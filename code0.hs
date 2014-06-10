module Test where

import Control.Applicative

-- <<Fetch
data Fetch a = Done a | Blocked (Fetch a)
-- >>

-- <<Functor
instance Functor Fetch where
  fmap f (Done x) = Done (f x)
  fmap f (Blocked c) = Blocked (fmap f c)
-- >>

-- <<Monad
instance Monad Fetch where
  return = Done

  Done a    >>= k = k a
  Blocked c >>= k = Blocked (c >>= k)
-- >>

-- <<Applicative
instance Applicative Fetch where
  pure = return

  Done g    <*> Done y    = Done (g y)
  Done g    <*> Blocked c = Blocked (g <$> c)
  Blocked c <*> Done y    = Blocked (c <*> Done y)
  Blocked c <*> Blocked d = Blocked (c <*> d)
-- >>

{-

identity
========

     pure id <*> v = v

  Done id <*> v
=> { if v == Done x }
  Done id <*> Done x
=>
  Done (id x)
=>
  Done x

=> { if v == Blocked c }
  Done id <*> Blocked c
=>
  Blocked (id <$> c)
=>
  Blocked c


homomorphism
============

     pure f <*> pure x = pure (f x)

  Done f <*> Done x
=>
  Done (f x)

interchange
===========

     u <*> pure y = pure ($ y) <*> u

Structural induction on u
  u = Done f | Blocked c
  induction hypothesis: c <*> pure y = pure ($ y) <*> c

Case 1: u = Done f

  u <*> Done y
=>
  Done f <*> Done y
=>
  Done (f y)

  pure ($ y) <*> u
=>
  Done ($ y) <*> Done f
=>
  Done (($ y) f)
=>
  Done (f y)

Case 2: u = Blocked c

  Blocked c <*> Done y = Done ($ y) <*> Blocked c
=>
  Blocked (c <*> Done y) = Blocked (Done ($ y) <*> c)
=> by induction hypothesis


composition
===========

     pure (.) <*> u <*> v <*> w = u <*> (v <*> w)

Structural induction on u

Case 1: u = Done f

  Done (.) <*> Done f <*> v <*> w = Done f <*> (v <*> w)
=>
  Done (f .) <*> v <*> w = Done f <*> (v <*> w)

Structural induction on v

Case 1.1 v = Done g

  Done (f .) <*> Done g <*> w = Done f <*> (Done g <*> w)
=>
  Done (f . g) <*> w = Done f <*> (Done g <*> w)

Structural induction on w

Case 1.1.1 w = Done x

  Done (f . g) <*> Done x = Done f <*> (Done g <*> Done x)
=> {
  Done ((f . g) x) = Done (f (g x))

Case 1.1.2 w = Blocked c

  Done (f . g) <*> Blocked c = Done f <*> (Done g <*> Blocked c)
=>
  Blocked ((f . g) <*> c) = Blocked (f <$> g <*> c)
=> { induction: Done (f . g) <*> c = Done f <*> (Done g <*> c)

Structural induction on v

Case 1.2 v = Blocked c
  induction (1.2): Done (f .) <*> c <*> w = Done f <*> (c <*> w) )

  Done (f .) <*> Blocked c <*> w = Done f <*> (Blocked c <*> w)
=>
  Blocked ((f .) <$> c) <*> w = Done f <*> (Blocked c <*> w)

Structural induction on w

Case 1.2.1 w = Done x

  Blocked ((f .) <$> c) <*> Done x = Done f <*> (Blocked c <*> Done x)
=>
  Blocked ((f .) <$> c <*> Done x) = Blocked (f <$> (c <*> Done x))
=> by (1.2)

Case 1.2.2 w = Blocked d
  induction (1.2.2): Blocked ((f .) <$> c) <*> d = Done f <*> (Blocked c <*> d)

  Blocked ((f .) <$> c) <*> Blocked d = Done f <*> (Blocked c <*> Blocked d)
=>
  Blocked ((f .) <$> c <*> d) = Blocked (f <$> c <*> d)
=> by (1.2.2)

Case 2 u = Blocked c
  induction (2): Done (.) <*> c <*> v <*> w = c <*> (v <*> w)

  Done (.) <*> Blocked c <*> v <*> w = Blocked c <*> (v <*> w)
=>
  Blocked ((.) <$> c) <*> v <*> w = Blocked c <*> (v <*> w)

Structural induction on v

Case 2.1 v = Done f

  Blocked ((.) <$> c) <*> Done f <*> w = Blocked c <*> (Done f <*> w)
=>
  Blocked ((.) <$> c <*> Done f) <*> w = Blocked c <*> (Done f <*> w)

Structural induction on w

Case 2.1.1 w = Done x

  Blocked ((.) <$> c <*> Done f) <*> Done g = Blocked c <*> (Done f <*> Done g)
=>
  Blocked ((.) <$> c <*> Done f <*> Done x) = Blocked (c <*> (Done f <*> Done g))
=> by (2)

Case 2.1.2 w = Blocked d
  induction (2.1.2): Done (.) <*> c <*> Done f <*> d = c <*> (Done f <*> d)

  Blocked ((.) <$> c <*> Done f) <*> Blocked d = Blocked c <*> (Done f <*> Blocked d)
=>
  Blocked ((.) <$> c <*> Done f <*> d) = Blocked (c <*> (Done f <*> d))
=> by (2.1.2)

Case 2.2 v = Blocked d
  induction (2.2) Done (.) <*> c <*> d <*> w = c <*> (d <*> w)

  Blocked ((.) <$> c) <*> Blocked d <*> w = Blocked c <*> (Blocked d <*> w)
=>
  Blocked ((.) <$> c <*> d) <*> w = Blocked c <*> (Blocked d <*> w)

Structural induction on w

Case 2.2.1 w = Done x

  Blocked ((.) <$> c <*> d) <*> Done x = Blocked c <*> (Blocked d <*> Done x)
=>
  Blocked ((.) <$> c <*> d <*> Done x) = Blocked (c <*> (d <*> Done x))
=> by (2.2)

Case 2.2.2 w = Blocked e
  induction (2.2.2): Done (.) <*> c <*> d <*> e = c <*> (d <*> e)

  Blocked ((.) <$> c) <*> Blocked d <*> Blocked e = Blocked c <*> (Blocked d <*> Blocked e)
=>
  Blocked ((.) <$> c <*> d <*> e) = Blocked (c <*> (d <*> e))
=> by (2.2.2)

-}

