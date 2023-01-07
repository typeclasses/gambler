This package defines the `Fold`, `NonemptyFold`, and `EffectfulFold` types and
provides an assortment of ways to construct, combine, and use them.

> Every gambler knows that the secret to surviving<br>
> Is knowing what to throw away and knowing what to keep

> You got to know when to hold 'em, know when to fold 'em<br>
> Know when to walk away, and know when to run

— *The Gambler* by Don Schlitz, popularized by Kenny Rogers


## Intro to Fold

The `foldl'` function in the `base` package is used when we want a strictly
evaluated result from traversing a list.

```haskell
foldl' :: Foldable t => (b -> a -> b) -> b -> t a -> b
```

For example, to sum a list of numbers:

```ghci
λ> import qualified Data.List as List

λ> List.foldl' (+) 0 [1..100]
5050
```

What if we put the first two parameters to `List.foldl'` into a datatype?

```haskell
data Fold a b = Fold
    { initial :: b
    , step :: b -> a -> b }
```

Or, better yet, we can use a trick to turn the datatype into a `Functor` (which
will become important when we discuss the `Applicative` a bit later):

```haskell
data Fold a b = forall x. Fold
    { initial :: x
    , step :: x -> a -> x
    , extract :: x -> b }
```

We can then express the concept of numeric summation as:

```haskell
sum :: Num a => Fold a a
sum = Fold{ initial = 0, step = (+), extract = id }
```

This `Fold` can be used to sum lists and other `Foldable` collections, but it
can also be used to sum effectful streams. So even without any further
mechanism, just having this datatype gives us some useful expressive power.
There is no need for each streaming library to duplicate all the work of
defining its own copies of `sum`, `product`, `all`, `any`, `and`, `or`,
`minimum`, `maximum`, etc.; a library that provides some kind of `Stream` type
needs only define a function to apply a fold to a stream ...

```haskell
foldStream :: Fold a b -> Stream m a -> m b
```

... and then users can make use of any library of folds that they may find or
concoct. `gambler` itself contains much of the functionality of the standard
`Data.List` module, but there are more things in heaven and earth than are
dreamt of in this package.


## Intro to NonemptyFold

There are some kinds of folding that only work if the input it nonempty.
Suppose, for example, we want the greatest of all the items. If there are no
items, there is no greatest item. We express this sort of thing with a slight
modification to `Fold`:

```haskell
data NonemptyFold a b = forall x. NonemptyFold
    { initial :: a -> x
    , step :: x -> a -> x
    , extract :: x -> b }
```

The only thing that's different is the type of the `initial` field has changed
from `x` to `a -> x`; it is now parameterized on the first item.

The notion of selecting greatest item can now be expressed as:

```haskell
maximum = NonemptyFold{ initial = id, step = max, extract = id }
```

A `NonemptyFold` can be converted to a `Fold` using `Fold.Pure.nonemptyFold`.
The conversion changes the fold's return type from `b` to `Maybe b` to
accommodate the possibility of empty input.


## Intro to EffectfulFold

There is a related function in `base` that does the same thing as `foldl'` but
in a monadic context:

```haskell
foldM :: Foldable t => Monad m => (b -> a -> m b) -> b -> t a -> m b
```

This allows us to perform effects as we fold.

```
λ> import qualified Control.Monad as Monad

λ> Monad.foldM (\x a -> putStrLn ("* " <> show a) $> (x + a)) 0 [1..5]
* 1
* 2
* 3
* 4
* 5
15
```

The type we define corresponding to the arguments of `Monad.foldM` is:

```haskell
data EffectfulFold m a b = forall x. EffectfulFold
    { initial :: m x
    , step :: x -> a -> m x
    , extract :: x -> m b }
```

A regular `Fold` can be converted to an `EffectfulFold` of any monad using
`Fold.Effectful.fold`.


## The Applicative instances

The `Fold` and `EffectfulFold` applicatives are great for computing multiple folds
over a collection in one pass over the data. For example, suppose that you want
to compute both the sum and the length of a list. The following approach works,
but it uses space inefficiently:

```haskell
import qualified Data.List as List

sumAndLength :: Num a => [a] -> (a, Natural)
sumAndLength xs = (List.sum xs, List.genericLength xs)
```

The problem is this goes over the list in two passes. If you demand the result
of `sum`, the Haskell runtime will materialize the entire list. However, the
runtime cannot garbage collect the list because the list is still required for
the call to `length`. The space requirement of `sumAndLength` is therefore
linear with respect to the size of the list. We can do much better.

With `gambler`, we can instead write:

```haskell
import qualified Fold

sumAndLength :: Num a => [a] -> (a, Natural)
sumAndLength = Fold.runFold $ (,) <$> Fold.sum <*> Fold.length
```

This achieves the same result using constant space.


## Quick start

To get quickly playing around with `gambler`, launch GHCi using `cabal`:

```bash
cabal repl --build-depends gambler
```

The example from the previous section can be run as follows:

```haskell
λ> import qualified Fold
```

```haskell
λ> Fold.runFold ((,) <$> Fold.sum <*> Fold.length) [1..1000000]
(500000500000,1000000)
```


## Related packages

This `gambler` package is mostly a copy of [foldl], with some features removed
to minimize its dependency set. What remains in `gambler` is essentially the
same as what can be found in `foldl` version `1.4.13`, subject only to
reorganization, renaming, and minor modifications.

  [foldl]: https://hackage.haskell.org/package/foldl


## Future plans

Once the `Foldable1` class has been added to `base`, the type of
`Fold.Nonempty.run` may be generalized to accommodate it.