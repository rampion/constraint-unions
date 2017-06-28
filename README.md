# Setup 
Since this is a literate haskell file, we need to specify all our language extensions up front.

```haskell
{-# OPTIONS_GHC -Wall -Werror -Wno-name-shadowing #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
module ConstraintUnions where

import Data.Typeable

class c || d where
  resolve :: (c => r) -> (d => r) -> r
infixr 2 ||

{-
-- Duplicate instances
instance {-# OVERLAPPABLE #-} d => (c || d) where resolve _ r = r
instance {-# OVERLAPPING #-} c => (c || d) where resolve r _ = r
-}

instance ((c0 || d), (c1 || d)) => (c0, c1) || d where
  resolve = resolve @c0 @d (resolve @c1 @d inLeft inRight) inRight where

    inLeft :: forall c d r. c => (c => r) -> (d => r) -> r
    inLeft r _ = r

    inRight :: forall c d r. d => (c => r) -> (d => r) -> r
    inRight _ r = r

type MaybeC c = c || ()
type p? a = MaybeC (p a)

given :: forall c x. MaybeC c => (c => x) -> x -> x
given = resolve @c @() inJust inNothing where

  inJust :: forall c r. c => (c => r) -> r -> r
  inJust r _ = r

  inNothing :: forall c r. (c => r) -> r -> r
  inNothing _ r = r

----

{-|-----------------------------------------------------------------------------
>>> showAny0 (5 :: Int)
"5"
>>> showAny0 "five"
"\"five\""
>>> showAny0 $ (+) @Int
"_"
-}
showAny0 :: forall a. Show? a => a -> String
showAny0 = given @(Show a) show (const "_")

instance (Show Int || d) where resolve = \r _ -> r
instance (Show String || d) where resolve = \r _ -> r
instance d => (Show (a -> b) || d) where resolve = \_ r -> r

showTypeOf :: forall a. Typeable a => a -> String
showTypeOf = const (show . typeRep $ Proxy @a)

{-|-----------------------------------------------------------------------------
>>> showAny1 (5 :: Int)
"5"
>>> showAny1 "five"
"\"five\""
>>> showAny1 $ (+) @Int
"_ :: Int -> Int -> Int"
-}
showAny1 :: forall a. (Show? a, Typeable? a) => a -> String
showAny1 = given @(Show a) show
         . given @(Typeable a) (showString "_ :: " . showTypeOf)
         $ const "_"

instance (Typeable Int || d) where resolve = \r _ -> r
instance (Typeable String || d) where resolve = \r _ -> r
instance (Typeable a || d, Typeable b || d) => (Typeable (a -> b) || d) where
  resolve = resolve @(Typeable a, Typeable b) @d

{-|-----------------------------------------------------------------------------
>>> showAny2 (5 :: Int)
"5"
>>> showAny2 "five"
"\"five\""
>>> showAny2 $ (+) @Int
"_ :: Int -> Int -> Int"
-}
showAny2 :: forall a. (Show a || Typeable a) => a -> String
showAny2 = resolve @(Show a) @(Typeable a) 
             show 
             (showString "_ :: " . showTypeOf)


{-|-----------------------------------------------------------------------------
>>> (5 :: Int) `equalsString0` "5"
True
>>> (+) @Int `equalsString0` "(+)"
...
    • No instance for (Eq (Int -> Int -> Int))
        arising from a use of ‘equalsString0’
...
-}
equalsString0 :: forall a. (Show a || (Eq a, Read a)) => a -> String -> Bool
equalsString0 = resolve @(Show a) @(Eq a, Read a)
  (\a s -> show a == s)
  (\a s -> a == read s)

{-|-----------------------------------------------------------------------------
>>> :set -XTypeApplications
>>> (5 :: Int) `equalsString1` "5"
True
>>> (+) @Int `equalsString1` "(+)"
...
    • No instance for (Show (Int -> Int -> Int))
        arising from a use of ‘equalsString1’
...
-}
equalsString1 :: forall a. ((Eq a, Read a) || Show a) => a -> String -> Bool
equalsString1 = resolve @(Eq a, Read a) @(Show a)
  (\a s -> a == read s)
  (\a s -> show a == s)

instance (Eq Int || d) where resolve = \r _ -> r
instance d => (Eq (a -> b) || d) where resolve = \_ r -> r

instance (Read Int || d) where resolve = \r _ -> r
instance d => (Read (a -> b) || d) where resolve = \_ r -> r
```

# Literate Haskell

This README.md file is a literate haskell file, for use with [`markdown-unlit`](https://github.com/sol/markdown-unlit#readme).
To allow GHC to recognize it, it's softlinked as `Kinder/Functor.lhs`, which you can compile with

    $ ghc -pgmL markdown-unlit Kinder/Functor.lhs

Many of the above examples are [`doctest`](https://github.com/sol/doctest#readme)-compatible, and can be run with

    $ doctest -pgmL markdown-unlit Kinder/Functor.lhs

Alternately, you can have cabal manage the dependencies and compile and test this with:

    $ cabal install --dependencies-only --enable-tests
    $ cabal build
    $ cabal test
