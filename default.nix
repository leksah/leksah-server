{ mkDerivation, aeson, attoparsec, base, base-compat, binary
, binary-shared, bytestring, Cabal, conduit, conduit-extra
, containers, deepseq, directory, doctest, executable-path
, filepath, ghc, ghc-boot, haddock-api, haddock-library, hslogger
, HTTP, HUnit, ltk, network, network-uri, parsec, pretty, process
, resourcet, stdenv, strict, text, time, transformers, unix
}:
mkDerivation {
  pname = "leksah-server";
  version = "0.16.1.0";
  src =
    builtins.filterSource (path: type: # FIXME: How to re-use .gitignore? https://git.io/vSo80
      nixpkgs.lib.all (i: toString i != path) [ ./.DS_Store ./default.nix ]
        && nixpkgs.lib.all (i: i != baseNameOf path) [ ".git" "dist-newstyle" "cabal.project.local" "dist" ".stack-work" ".vagrant" ".DS_Store" ]
        && nixpkgs.lib.all (i: !(nixpkgs.lib.hasSuffix i path)) [ ".lkshf" ]
        && nixpkgs.lib.all (i: !(nixpkgs.lib.hasPrefix i path)) [ ".ghc.environment." ]
        # TODO: what else?
      ) ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson attoparsec base base-compat binary binary-shared bytestring
    Cabal conduit conduit-extra containers deepseq directory
    executable-path filepath ghc ghc-boot haddock-api haddock-library
    hslogger HTTP ltk network network-uri parsec pretty process
    resourcet strict text time transformers unix
  ];
  executableHaskellDepends = [
    attoparsec base base-compat binary binary-shared bytestring Cabal
    conduit conduit-extra containers deepseq directory executable-path
    filepath ghc ghc-boot haddock-api haddock-library hslogger HTTP ltk
    network network-uri parsec pretty process resourcet strict text
    time transformers unix
  ];
  testHaskellDepends = [
    base conduit conduit-extra directory doctest filepath hslogger
    HUnit process resourcet text transformers
  ];
  homepage = "http://leksah.org";
  description = "Metadata collection for leksah";
  license = "GPL";
}
