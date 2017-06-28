# Constraint Unions

The humble comma. In haskell it does many jobs, for values, types, and
constraints. The category theorists will tell you that the simple type `(a,b)`
is a *product*, justifying the metaphor by if type `a` has `n` values, and type
`b` has `m` values, then type `(a,b)` has exactly `n * m` values.

The same goes for constraints. The constraint `(c,d)` is also a product, but since
we think of constraints in more binary terms (they either hold or they don't), we can
also say that it's an *intersection* of those two constraints.

In addition to product types, Haskell also has *sum* types, sometimes called
*tagged unions* in other languages.  `Either a b` is the canonical example -
justifying the "sum" metaphor by if type `a` has `n` values, and type `b` had
`m` values, then type `Either a b` has exactly `n + m`.

`Either` can be used as a building block for other sum types:
* `Bool` is isomorphic to `Either () ()`
* `Maybe a` is isomorphic to `Either () a`
* `Ordering` is isomorphic to `Either () (Either () ())`

What I'm going to do in this article is define a constraint-level equivalent
of `Either` to describe the *union* of two constraints.

# Setup
Since this is a literate haskell file, we need to specify all our language
extensions and imports up front.

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
import Prelude hiding (Either(..), either) -- only for didactic reasons
import Data.Typeable -- only used for one of the examples
```

# A closer look at `Either`

Haskell's algebraic data types inherently support sum and product types,
so `Either` is defined as a normal type:

```haskell
data Either a b = Left a | Right b
```

`Either a b` values can be consumed via pattern matching, but there's also
the handy `either` function in `Prelude` that can do the matching for you:

```haskell
either :: (a -> r) -> (b -> r) -> Either a b -> r
either f _ (Left a) = f a
either _ g (Right b) = g b
```

If we swap the argument order around, we can convert `Either` to its [church
encoding](https://en.wikipedia.org/wiki/Church_encoding) and back again:

```haskell
type ChurchEither a b = forall r. (a -> r) -> (b -> r) -> r

toChurchEither :: Either a b -> ChurchEither a b
toChurchEither e f g = either f g e

ofChurchEither :: ChurchEither a b -> Either a b
ofChurchEither c = c Left Right
```

# The `||` class

We'll use this church encoding of `Either` to define our constraint level
union, the `||` class.

```haskell
class c || d where
  resolve :: (c => r) -> (d => r) -> r
infixr 2 ||
```

The name may seem a little cute over something a bit more verbose like `EitherC`, but
it leads to clearly understood code in context, so I like it.

In addition, using `||` borrows from the same boolean metaphor that I normally
read constraints with. It's also commonly associated with a left-to-right bias,
which will also apply to our typeclass as we'll see below.

At this point, the tempting thing is to define two instances and be done with it:

    instance {-# OVERLAPPABLE #-} d => (c || d) where resolve _ r = r
    instance {-# OVERLAPPING #-} c => (c || d) where resolve r _ = r

The logical statements `d => (c || d)` and `c => (c || d)` are fairly well
understood, and letting the latter override the former is consistent
with the common use of `||` in many programming languages from the C-family
and beyond. Why require `d` if you've got `c`?

GHC, however, would have none of this.  When resolving instances it has no
notion of what's on the left-hand side of the `=>` in the instance declarations
so the two instances it sees are for `(c || d)` and `(c || d)`.  These aren't
merely overlapping, they're duplicates, and it has no idea how to choose
between them.

With that road closed off, it's not immediately obvious what clever trick to
use to define sensible instances for `||`, so let's table instances for a
moment, and consider how we might use the class instead.

# Example: comparing a string and a value

Suppose we're feeling nostalgic for perl, and we want to compare a string and a
value for equality.  There's two obvious ways to define such equality:
* use `show` to convert the value to a `String`, and use `String`'s definition of equality,
* use `read` to convert the `String` to a value, and use the value type's definition of equality.

Either one would work, for non-surprisingly for this point in the paper, we've
got a perfect example function:

```haskell
equalsString0 :: forall a. (Show a || (Eq a, Read a)) => a -> String -> Bool
equalsString0 = resolve @(Show a) @(Eq a, Read a)
  (\a s -> show a == s)
  (\a s -> a == read s)
```

GHC needed some help figuring out what instance of `resolve` we needed, so we
used `TypeApplications` to tell what constraints we were choosing between.

With a little light testing, `equalsString0` seems to give us the results
we expect on good inputs, and not horrible error messages on bad ones:

```haskell
{-$-----------------------------------------------------------------------------
>>> (5 :: Int) `equalsString0` "5"
True
>>> (5 :: Int) `equalsString0` "5.0"
False
>>> (+) @Int `equalsString0` "(+)"
...
    • No instance for (Eq (Int -> Int -> Int))
        arising from a use of ‘equalsString0’
...
-}
```

Of course, to actually call it, we had to define some instances for `||`:

```haskell
instance (Show Int || d) where resolve = \r _ -> r
instance d => (Show (a -> b) || d) where resolve = \_ r -> r
```

Structurally, these are very similar to the duplicate instances attempted above
except that `c` is fully specified here.

In fact, these two fit the boilerplate we'll be using for instances `c || d`
for unqualified `c`.  If we know `c` always holds, we'll use the former, and if
we know `c` never holds, we'll use the latter.  Note that neither specifies `d`,
so we avoid needing quadratic number of instances of `||`, which trend we will
continue.

For those of you already sucking on your teeth at the word "boilerplate", I
promise to return to this later.

Returning to the `equalsString0`, I may have spoke too soon when it comes to
testing:

```haskell
instance (Show Float || d) where resolve = \r _ -> r

{-$-----------------------------------------------------------------------------
>>> (5 :: Float) `equalsString0` "5.0"
True
>>> (5 :: Float) `equalsString0` "5"
False
-}
```

`Float` has both a `Show` instance as well as `Eq` and `Read` instances, but
due to the `||` instances' left-to-right bias, we used the `Show` branch to
compare "5.0" and "5" in the second example above.

We can define a different function that prefers to compare values to strings:

```haskell
equalsString1 :: forall a. ((Eq a, Read a) || Show a) => a -> String -> Bool
equalsString1 = resolve @(Eq a, Read a) @(Show a)
  (\a s -> a == read s)
  (\a s -> show a == s)
```

And now we get different results on `Float` values than before:

```haskell
instance (Eq Float || d) where resolve = \r _ -> r
instance (Read Float || d) where resolve = \r _ -> r

{-$-----------------------------------------------------------------------------
>>> (5 :: Float) `equalsString1` "5"
True
>>> (5 :: Float) `equalsString1` "5.000"
True
-}
```

While still getting sensible errors:

```haskell
instance d => (Eq (a -> b) || d) where resolve = \_ r -> r
instance d => (Read (a -> b) || d) where resolve = \_ r -> r

{-$-----------------------------------------------------------------------------
>>> (+) @Int `equalsString1` "(+)"
...
    • No instance for (Show (Int -> Int -> Int))
        arising from a use of ‘equalsString1’
...
-}
```

Though we do need to define our first *non*-boilerplate instance of `||` in
order to tell GHC that we can derive the union of an intersection from the
intersection of unions:

```haskell
instance ((c0 || d), (c1 || d)) => (c0, c1) || d where
  resolve = resolve @c0 @d (resolve @c1 @d inLeft inRight) inRight where

    inLeft :: forall c d r. c => (c => r) -> (d => r) -> r
    inLeft r _ = r

    inRight :: forall c d r. d => (c => r) -> (d => r) -> r
    inRight _ r = r
```



Returning to `equalsString1`, more testing reveals that it has a new
bug that `equalsString0` didn't:

```haskell
instance (Eq Int || d) where resolve = \r _ -> r
instance (Read Int || d) where resolve = \r _ -> r

{-$-----------------------------------------------------------------------------
>>> (5 :: Int) `equalsString1` "5"
True
>>> (5 :: Int) `equalsString1` "5.000"
*** Exception: Prelude.read: no parse
-}
```

Curse of the partial `read`!

We could try to catch the exception, and default to using string comparison if
we get a parse error, which means we'd need a type like

    equalsStringX :: forall a. ((Eq a, Read a, Show a) || Show a) => a -> String -> Bool

If we look at this constraint algebraically, as sums and products, we have:

    Eq * Read * Show + Show

Which, by highschool algebra we know is:

    (Eq * Read + 1) * Show

The sum type `a + 1` has a more familiar name in Haskell - `Maybe a`, so we
just want the constraint-level equivalent:

    equalsStringX :: forall a. (MaybeC (Eq a, Read a), Show a) => a -> String -> Bool

# `MaybeC`

Just as we can define `Maybe` in terms of `Either`, we can define `MaybeC` in terms
of `||`.

```haskell
type MaybeC c = c || ()
```

GHC needs a little help to figure out that `() => a` is isomorphic to `a`, so
we provide a helper function that does just that:

```haskell
given :: forall c r. MaybeC c => (c => r) -> r -> r
given = resolve @c @() inJust inNothing where

  inJust :: forall c r. c => (c => r) -> r -> r
  inJust r _ = r

  inNothing :: forall c r. (c => r) -> r -> r
  inNothing _ r = r
```

`MaybeC` is pretty often used with only one typeclass, so we can inject a
little syntactical sugar for that case:

```haskell
type p? a = MaybeC (p a)
```

(If you thought `||` was too cute a name, you're going to hate `?`)

Rather than returning to `equalsStringX` to wrestle with `catch` and `unsafePerformIO`,
let's use a new example to examine `MaybeC`.

# Example: print ALL the values!!!

Here's a simple idea - a function that yields a string for any value, even if
its type doesn't have a `Show` instance.  Obviously we want to use the `Show`
instance if we can, but if none is defined, we'll just print some sensible
default value.

```haskell
showAny0 :: forall a. Show? a => a -> String
showAny0 = given @(Show a) show (const "_")

{-$-----------------------------------------------------------------------------
>>> showAny0 (5 :: Int)
"5"
>>> showAny0 $ (+) @Int
"_"
-}
```

(No new instances required! Our previous boilerplate suffices.)

That `_` is rather lackluster, especially since for many types
we can use `Typeable` to print the type itself, so let's add that:

```haskell
showTypeOf :: forall a. Typeable a => a -> String
showTypeOf = const (show . typeRep $ Proxy @a)

showAny1 :: forall a. (Show? a, Typeable? a) => a -> String
showAny1 = given @(Show a) show
         . given @(Typeable a) (showString "_ :: " . showTypeOf)
         $ const "_"

instance (Typeable Int || d) where resolve = \r _ -> r
instance (Typeable a || d, Typeable b || d) => (Typeable (a -> b) || d) where
  resolve = resolve @(Typeable a, Typeable b) @d

{-$-----------------------------------------------------------------------------
>>> showAny1 (5 :: Int)
"5"
>>> showAny1 $ (+) @Int
"_ :: Int -> Int -> Int"
-}
```

Here, the `Typeable (a -> b) || d` instance deviates from the boilerplate
instances we've seen so far.  This is the boilerplate for a conditional
constraint - one that depends on other constraints (the ones we've seen thus
far have been unqualified).  `a -> b` is `Typeable` only if `a` and `b` are,
and this instance reflects that by reusing the "intersection of unions"
instance from above.

# About those boilerplate instances

Nobody likes boilerplate, and that is a major drawback. But as complaints go,
the monotony's only the tip of the iceberg.

Though may have avoided being quadratic in the number of constraints by 
never specifying the second constraint in the boilerplate, and avoided
needing to define countably infinite instances by breaking products
of constraints down, to be complete, we're still linear in the number
of possible constraints - not just the ones that hold.

This means, for completeness, for just the single-parameter typeclasses, we
need to define one instance for every possible datatype.  For multi-parameter
typeclasses it's worse.

And that's even before you consider the question of where these instances live.
If they all live in the module that defines `||`, then the package containing
that module has a dependency on every other package that defines a datatype or
a typeclass.

Sigh.

We *can* avoid defining a quadratic number of boilerplate instances for the
`~`-equality constraints by using overlapping instances:

```haskell
instance {-# OVERLAPPABLE #-} d => (a ~ b) || d where resolve = \_ a -> a
instance {-# OVERLAPPING #-} (a ~ a) || d where resolve = \a _ -> a

{-$-----------------------------------------------------------------------------
>>> given @(Int ~ Char) True False
False
>>> given @(Char ~ Char) True False
True
-}
```

The alternative to generating the boilerplate instances by hand is to have GHC
generate them for us.  After all, writing code we don't want to write is what a
compiler's for, and it's in a position to see which instances are actually needed,
so it doesn't need to blithely implement all possible instances.

However, this is some new `LANGUAGE` pragma type dreaming, and I'm still not
certain that the only boilerplate instances are the three used above:

    -- constraint `C` holds unconditionally, i.e. we have `instance C where...`
    instance (C || d) where resolve = \a _ -> a

    -- constraint `C` holds conditionally, i.e. we have `instance C' => C where...`
    instance (C' || d) => (C || d) where resolve = resolve @C' @d

    -- constraint `C` holds under no conditions
    instance d => (C || d) where resolve = \_ a -> a


# Literate Haskell

This README.md file is a literate haskell file, for use with [`markdown-unlit`](https://github.com/sol/markdown-unlit#readme).
To allow GHC to recognize it, it's softlinked as `ConstraintUnions.lhs`, which you can compile with

    $ ghc -pgmL markdown-unlit ConstraintUnions.lhs

Many of the above examples are [`doctest`](https://github.com/sol/doctest#readme)-compatible, and can be run with

    $ doctest -pgmL markdown-unlit ConstraintUnions.lhs

Alternately, you can have cabal manage the dependencies and compile and test this with:

    $ cabal install --dependencies-only --enable-tests
    $ cabal build
    $ cabal test
