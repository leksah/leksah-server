{ leksah-server, pkg }:
let toList = x: if builtins.isList x then x else [x];
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
