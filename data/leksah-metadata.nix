{ ghc, pkg }:
let toList = x: if builtins.isList x then x else [x];
    haskellLib = (import <nixpkgs> {}).pkgs.haskell.lib;
    leksah-server = (ghc.extend (self: super: {
        haddock-library = haskellLib.dontCheck (haskellLib.dontHaddock super.haddock-library);
        haddock-api = haskellLib.dontCheck (haskellLib.dontHaddock super.haddock-api);
        leksah-server = haskellLib.dontCheck (self.callCabal2nix "leksah-server" ((import <nixpkgs> {}).fetchFromGitHub {
          owner = "leksah";
          repo = "leksah-server";
          rev = "d02bce09f22ff4a028b6f9585632cea08181a304";
          sha256 = "19dg61q6qxndczm38jvk41a6s06b4wkg221jcaw6zqvfypkjbm19";
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
