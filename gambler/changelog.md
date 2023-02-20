## 0.1.0.0

Added modules:

- `Fold.Shortcut`
- `Fold.Shortcut.Type`
- `Fold.Shortcut.Run`
- `Fold.Shortcut.Examples`
- `Fold.Shortcut.Conversion`
- `Fold.Shortcut.ShortcutNonempty`

- `Fold.ShortcutNonempty`
- `Fold.ShortcutNonempty.Type`
- `Fold.ShortcutNonempty.Run`
- `Fold.ShortcutNonempty.Examples`
- `Fold.ShortcutNonempty.Conversion`
- `Fold.ShortcutNonempty.Shortcut`

- `Fold.Pure.ShortcutNonempty`

- `Fold.Effectful.ShortcutNonempty`

- `Fold.Nonempty.ShortcutNonempty`

Additions to existing modules:

- `Fold.Pure` (`shortcutFold`, `shortcutNonemptyFold`)
- `Fold.Pure.Conversion` (`shortcutFold`, `shortcutNonemptyFold`)

- `Fold.Nonempty` (`shortcutFold`, `shortcutNonemptyFold`)
- `Fold.Nonempty.Conversion` (`shortcutFold`, `shortcutNonemptyFold`)

- `Fold.Effectful` (`shortcutFold`, `shortcutNonemptyFold`)
- `Fold.Effectful.Conversion` (`shortcutFold`, `shortcutNonemptyFold`)

Removed:

- `Fold.Effectful.Nonempty` (`first`)
- `Fold.Nonempty.Examples` (`first`)
- `Fold.Pure.Nonempty` (`first`)

Type changes:

- `Fold.first` changed from `NonemptyFold a a` to `ShortcutNonemptyFold a a`

## 0.0.1.0 (2023-02-20)

Add `Fold.Nonempty.effectfulFold`; this was already available from
`Fold.Nonempty.Conversion`, but now it is also re-exported from `Fold.Nonempty`.

## 0.0.0.1 (2023-02-17)

Remove benchmark, which was broken

## 0.0.0.0 (2023-01-07)

Initial release
