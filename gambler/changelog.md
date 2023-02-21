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
