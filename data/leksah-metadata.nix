{ ghc, pkg }:
let toList = x: if builtins.isList x then x else [x];
    haskellLib = (import <nixpkgs> {}).pkgs.haskell.lib;
    leksah-server = (ghc.extend (self: super: {
        haddock-library = haskellLib.dontCheck (haskellLib.dontHaddock super.haddock-library);
        haddock-api = haskellLib.dontCheck (haskellLib.dontHaddock super.haddock-api);
        leksah-server = haskellLib.dontCheck (self.callCabal2nix "leksah-server" ((import <nixpkgs> {}).fetchFromGitHub {
          owner = "leksah";
          repo = "leksah-server";
          rev = "6e6a02923cbc636dc589efff4e24ada7ddfd4093";
          sha256 = "00p508zh113acxds76i9r7fmi8pg5sblyz22d5v54z82j5h78g6q";
        }) {});
    })).leksah-server;
in pkg.overrideAttrs (old: {
    buildInputs = (old.buildInputs or []) ++ [pkg];
    configureFlags = toList (old.configureFlags or []) ++ ["--ghc-options=-fno-code"];
    outputs = ["out"];
    buildPhase = "(${old.buildPhase}) || true";
    checkPhase = "echo Skipping checkPhase";
    preInstallPhases = [];
    installPhase = ''
      echo Making Leksah Metadata
      ghc-pkg list
      PATH=${leksah-server}/bin:$PATH ${leksah-server}/bin/leksah-server -i $PWD -m $out/share/leksah/metadata --verbosity=DEBUG
      (rm Setup configure || true);
      find ./dist -type f ! -name '*.hs' -delete
      mkdir -p $out/share/leksah/packageSource
      cp -r * $out/share/leksah/packageSource/
    '';
    preFixupPhases = [];
    fixupPhase = "echo Skipping fixPhase";
    installCheckPhase = "echo skippingInstallCheckPhase";
    preDistPhases = [];
    distPhase = "echo Skipping distPhase";
    postPhases = [];
})
