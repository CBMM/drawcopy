{ mkDerivation, aeson, aeson-pretty, base, bifunctors, bytestring, ghcjs-base
, ghcjs-dom, lens, mtl, mwc-random, random, reflex, reflex-dom
, stdenv, string-qq, tagging-common, text, time, transformers
}:
mkDerivation {
  pname = "drawcopy";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson aeson-pretty base bifunctors bytestring ghcjs-base ghcjs-dom lens mtl
    mwc-random random reflex reflex-dom string-qq tagging-common text time
    transformers
  ];
  license = stdenv.lib.licenses.bsd3;
}
