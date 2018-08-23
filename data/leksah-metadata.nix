{ ghc, pkg }:
let toList = x: if builtins.isList x then x else [x];
    leksah-server = (ghc.extend (self: super: {
        haddock-library = dontCheck (dontHaddock super.haddock-library)
        haddock-api = dontCheck (dontHaddock super.haddock-api)
        leksah-server = (import <nixpkgs> {}).pkgs.haskell.lib.dontCheck (self.callCabal2nix "leksah-server" ((import <nixpkgs> {}).fetchFromGitHub {
          owner = "leksah";
          repo = "leksah-server";
          rev = "901ae158f1ac0c0002484394a7283398ad446c1c";
          sha256 = "0xz34izmhjknlcv1jckpllmj47nh7k1j6hcpxmf9x1bacx3vqf1z";
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
