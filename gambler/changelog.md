## 0.4.1.0

`Will (..)` and `Vitality (..)` are now re-exported from
`Fold.Shortcut` and `Fold.ShortcutNonempty`; previously these
were only available from `Fold.Shortcut.Type` and
`Fold.ShortcutNonempty.Type`.

A new utility `repeatedly` has been added to `Fold.Pure`,
`Fold.Nonempty`, `Fold.Shortcut`, and `Fold.ShortcutNonempty`.

New utilities `motivate`, `premap`, and `withVitality` and a new
type alias `Vitality'` have been added to `Fold.Shortcut` and
`Fold.ShortcutNonempty`.

## 0.4.0.0 (2023-03-08)

Changed `ShortcutFold` from

```haskell
data ShortcutFold a b = forall x y. ShortcutFold
    { initial     :: Vitality x y
    , step        :: y -> a -> Vitality x y
    , extractDead :: x -> b
    , extractLive :: y -> b }
```

to

```haskell
data ShortcutFold a b = forall x y. ShortcutFold
    { initial :: Vitality x y
    , step    :: y -> a -> Vitality x y
    , extract :: Vitality x y -> b }
```

Changed `ShortcutNonemptyFold` from

```haskell
data ShortcutNonemptyFold a b = forall x y. ShortcutNonemptyFold
    { initial     :: a -> Vitality x y
    , step        :: y -> a -> Vitality x y
    , extractDead :: x -> b
    , extractLive :: y -> b }
```

to

```haskell
data ShortcutNonemptyFold a b = forall x y. ShortcutNonemptyFold
    { initial :: a -> Vitality x y
    , step    :: y -> a -> Vitality x y
    , extract :: Vitality x y -> b }
```

Added `Fold.Shortcut.duplicate` and `Fold.ShortcutNonempty.duplicate`.

(The type changes make the `duplicate` functions possible.)

## 0.3.0.0 (2023-03-07)

In the `Fold.ShortcutNonempty` module, the type of `list` and
`reverseList` has changed from `ShortcutNonemptyFold a [a]`
to `ShortcutNonemptyFold a (NonEmpty a)`.

## 0.2.0.0 (2023-02-22)

The "Examples" modules are no longer divided into "Interesting" and "Boring"
modules in the public API, because this leads to too many breaking releases.

`NonemptyFold` and `ShortcutNonemptyFold` now have their own `sum` and `product`
definitions instead of being lifted variants of the `Fold` and `ShortcutFold`
definitions. This makes it possible to use them with numeric types that do not
include additive or multiplicative identity values. For example, we now have a
test case which takes the sum over a non-empty list of positive integers. Since
a "positive integer" type does not include zero, previously this would result in
arithmetic underflow.

## 0.1.0.0 (2023-02-20)

Adds `ShortcutFold` and `ShortcutNonemptyFold`.

The following have changed from `Fold` to `ShortcutFold`:
`and`, `or`, `all`, `any`, `element`, `notElement`, `find`,
`lookup`, `index`, `findIndex`, `elementIndex`, `null`.

`first` has changed from `NonemptyFold` to `ShortcutNonemptyFold`.

## 0.0.1.0 (2023-02-20)

Add `Fold.Nonempty.effectfulFold`; this was already available from
`Fold.Nonempty.Conversion`, but now it is also re-exported from `Fold.Nonempty`.

## 0.0.0.1 (2023-02-17)

Remove benchmark, which was broken

## 0.0.0.0 (2023-01-07)

Initial release
