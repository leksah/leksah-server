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
  src = ./.;
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
