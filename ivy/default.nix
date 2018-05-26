{ mkDerivation, aeson, aeson-casing, base, bytestring, containers
, cryptonite, either, errors, indexed, lens, monad-logger, mtl
, optparse-applicative, parsec, parsec-numbers, pretty-simple
, process, stdenv, template-haskell, text
}:
mkDerivation {
  pname = "ivy";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-casing base bytestring containers cryptonite either
    errors indexed lens monad-logger mtl optparse-applicative parsec
    parsec-numbers pretty-simple process template-haskell text
  ];
  executableHaskellDepends = [
    base containers either errors lens monad-logger mtl
    optparse-applicative parsec parsec-numbers pretty-simple process
    template-haskell text
  ];
  description = "A Language and its compiler on Ethereum Virtual Machine";
  license = stdenv.lib.licenses.mit;
}
