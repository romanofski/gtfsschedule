{ mkDerivation, aeson, array, base, base16-bytestring, binary
, bytestring, containers, directory, fetchgit, filepath, lib, mtl
, parsec, syb, text, utf8-string, vector
}:
mkDerivation {
  pname = "protocol-buffers";
  version = "2.4.17";
  src = fetchgit {
    url = "https://github.com/k-bx/protocol-buffers.git";
    sha256 = "0hph7l78iwkz5bs6z3sdjgcgkczmwr1vydphdbkb8i3r281sq8x2";
    rev = "9b109f2fefa75f45911edc967db145505bfc7a1b";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    aeson array base base16-bytestring binary bytestring containers
    directory filepath mtl parsec syb text utf8-string vector
  ];
  homepage = "https://github.com/k-bx/protocol-buffers";
  description = "Parse Google Protocol Buffer specifications";
  license = lib.licenses.bsd3;
}
