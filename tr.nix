{ reflex-platform, ... }:
let
  nixpkgs = (import <nixpkgs> {});
  dontCheck = nixpkgs.pkgs.haskell.lib.dontCheck;
in
reflex-platform.ghcjs.override {
  overrides = self: super: { 
     servant-reflex      = dontCheck (self.callPackage (reflex-platform.cabal2nixResult ../servant-reflex) {});
     #servant             = dontCheck (self.callPackage (reflex-platform.cabal2nixResult deps/servant/servant) {});
  };
}
