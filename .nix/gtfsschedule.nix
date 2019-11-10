{ mkDerivation, base, bytestring, cassava, conduit, conduit-extra
, containers, directory, esqueleto, HStringTemplate, http-conduit
, http-types, ini, lifted-base, monad-control, monad-logger, mtl
, network, old-locale, optparse-applicative, persistent
, persistent-sqlite, persistent-template, protocol-buffers
, QuickCheck, resourcet, silently, stdenv, streaming-commons
, system-filepath, tasty, tasty-hunit, tasty-quickcheck, temporary
, text, time, transformers, transformers-base, unliftio-core
, utf8-string, xdg-basedir, zip-archive
}:
mkDerivation {
  pname = "gtfsschedule";
  version = "0.8.0.0";
  src = ./..;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring cassava conduit conduit-extra containers directory
    esqueleto HStringTemplate http-conduit http-types ini monad-control
    monad-logger mtl old-locale optparse-applicative persistent
    persistent-sqlite persistent-template protocol-buffers resourcet
    system-filepath temporary text time transformers unliftio-core
    utf8-string xdg-basedir zip-archive
  ];
  executableHaskellDepends = [
    base bytestring http-conduit ini mtl optparse-applicative
    protocol-buffers text
  ];
  testHaskellDepends = [
    base bytestring conduit conduit-extra containers directory
    lifted-base monad-logger network persistent persistent-sqlite
    protocol-buffers QuickCheck resourcet silently streaming-commons
    tasty tasty-hunit tasty-quickcheck temporary text time transformers
    transformers-base
  ];
  homepage = "http://github.com/romanofski/gtfsschedule#readme";
  description = "Be on time for your next public transport service";
  license = stdenv.lib.licenses.gpl3;
}
