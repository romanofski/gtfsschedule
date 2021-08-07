{ mkDerivation, alex, array, base, binary, bytestring, containers
, directory, filepath, haskell-src-exts, lib, mtl, parsec
, protocol-buffers, protocol-buffers-descriptor, utf8-string, fetchgit }:
mkDerivation {
  pname = "hprotoc";
  version = "2.4.17";
  src = fetchgit {
    url = "https://github.com/k-bx/protocol-buffers.git";
    sha256 = "0hph7l78iwkz5bs6z3sdjgcgkczmwr1vydphdbkb8i3r281sq8x2";
    rev = "9b109f2fefa75f45911edc967db145505bfc7a1b";
    fetchSubmodules = true;
  };
  # This is yikes!!! Couldn't find a better way to build the subdirectory.
  preConfigure = ''
    rm -rf Text protocol-buffers.cabal
    mv hprotoc/* .
  '';
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    array base binary bytestring containers directory filepath
    haskell-src-exts mtl parsec protocol-buffers
    protocol-buffers-descriptor utf8-string
  ];
  libraryToolDepends = [ alex ];
  executableHaskellDepends = [
    array base binary bytestring containers directory filepath
    haskell-src-exts mtl parsec protocol-buffers
    protocol-buffers-descriptor utf8-string
  ];
  executableToolDepends = [ alex ];
  homepage = "https://github.com/k-bx/protocol-buffers";
  description = "Parse Google Protocol Buffer specifications";
  license = lib.licenses.bsd3;
}
