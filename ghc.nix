{ reflex-platform, ... }:
let
  nixpkgs = (import <nixpkgs> {});
  dontCheck = nixpkgs.pkgs.haskell.lib.dontCheck;
in
reflex-platform.ghc.override {
  overrides = self: super: { 
     # servant              = dontCheck (self.callPackage (reflex-platform.cabal2nixResult deps/servant/servant) {});
     servant-reflex       = dontCheck (self.callPackage (reflex-platform.cabal2nixResult deps/servant-reflex) {});
  };
}
