{ reflex-platform, ... }:
let
  nixpkgs = (import <nixpkgs> {});
  dontCheck = nixpkgs.pkgs.haskell.lib.dontCheck;
in
reflex-platform.ghcjs.override {
  overrides = self: super: { 
     reflex-dom-contrib  = dontCheck (self.callPackage (reflex-platform.cabal2nixResult deps/tagging/deps/reflex-dom-contrib) {});
     tagging-common      = dontCheck (self.callPackage deps/tagging/tagging-common/default.nix { compilername = "ghcjs"; });
     servant             = dontCheck (self.callPackage (reflex-platform.cabal2nixResult deps/tagging/deps/servant-snap/deps/servant/servant) {});
     servant-docs        = dontCheck (self.callPackage (reflex-platform.cabal2nixResult deps/tagging/deps/servant-snap/deps/servant/servant-docs) {});
     yaml-ghcjs          = dontCheck (self.callPackage (reflex-platform.cabal2nixResult deps/tagging/deps/yaml-ghcjs) {});
     groundhog-th        = dontCheck (self.callPackage deps/tagging/deps/groundhog/groundhog-th { compilername = "ghcjs"; });
     groundhog-postgresql = null;
  };
}
