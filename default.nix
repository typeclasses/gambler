let

sources = import ./nix/sources.nix;
nixos-22-11 = import sources."nixos-22.11" {};
nixos-unstable = import sources."nixos-unstable" {};
inherit (nixos-22-11) haskell lib symlinkJoin;
inherit (lib) fold composeExtensions concatMap attrValues;

combineOverrides = old:
    fold composeExtensions (old.overrides or (_: _: { }));

sourceOverrides = haskell.lib.packageSourceOverrides {
    gambler = ./gambler;
};

ghc."9.2" = nixos-22-11.haskell.packages.ghc92.override (old: {
    overrides = combineOverrides old [
        sourceOverrides
        (new: old: {
        })
    ];
});

ghc."9.4" = nixos-unstable.haskell.packages.ghc94.override (old: {
    overrides = combineOverrides old [
        sourceOverrides
        (new: old: {
            hspec = new.callPackage ./nix/hspec-2.11.nix {};
            hspec-core = new.callPackage ./nix/hspec-core.nix {};
            hspec-discover = new.callPackage ./nix/hspec-discover.nix {};
        })
    ];
});

in

symlinkJoin {
    name = "gambler";
    paths = concatMap (x: [x.gambler]) (attrValues ghc);
} // {
    inherit ghc;
    pkgs = nixos-22-11;
}
