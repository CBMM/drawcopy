{ reflex-platform, ... }:
let


  nixpkgs = (import <nixpkgs> {});

  # lifted from http://github.com/reflex-frp/reflex-platform
  cabal2nixResult2 = src: nixpkgs.runCommand "cabal2nixResult" {
    buildCommand = ''
      cabal2nix file://"${src}" >"$out"
    '';
    buildInputs = with nixpkgs; [
      cabal2nix
    ];
    # Support unicode characters in cabal files
    ${if !nixpkgs.stdenv.isDarwin then "LOCALE_ARCHIVE" else null} = "${nixpkgs.glibcLocales}/lib/locale/locale-archive";
    ${if !nixpkgs.stdenv.isDarwin then "LC_ALL" else null} = "en_US.UTF-8";
  };

in
reflex-platform.ghcjs.override {
  overrides = self: super: { 
     # reflex-dom-contrib  = (self.callPackage ../tagging/deps/reflex-dom-contrib { compilername = "ghcjs"; });
     reflex-dom-contrib  = (self.callPackage (reflex-platform.cabal2nixResult ../tagging/deps/reflex-dom-contrib) {});
     tagging-common      = (self.callPackage ../tagging/tagging-common/default.nix { compilername = "ghcjs"; });
     servant             = (self.callPackage (reflex-platform.cabal2nixResult ../tagging/deps/servant-snap/deps/servant/servant) {});
     servant-docs        = (self.callPackage (reflex-platform.cabal2nixResult ../tagging/deps/servant-snap/deps/servant/servant-docs) {});
     yaml-ghcjs          = (self.callPackage (reflex-platform.cabal2nixResult ../tagging/deps/yaml-ghcjs) {});
     groundhog-th        = (self.callPackage ../tagging/deps/groundhog/groundhog-th { compilername = "ghcjs"; });
     groundhog-postgresql = null;
  };
}
