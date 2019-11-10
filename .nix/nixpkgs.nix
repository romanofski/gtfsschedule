{ compiler ? null, nixpkgs ? null}:

let
  compilerVersion = if isNull compiler then "ghc863" else compiler;
  haskellPackagesOverlay = self: super: with super.haskell.lib; {
    haskellPackages = super.haskell.packages.${compilerVersion}.override {
      overrides = hself: hsuper: {
        gtfsschedule = hsuper.callPackage ./gtfsschedule.nix { };
      };
    };
  };
  pkgSrc =
    if isNull nixpkgs
    then
    builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/d5291756487d70bc336e33512a9baf9fa1788faf.tar.gz";
      sha256 = "0i64wsl20fl92bsqn900nxmmnr1v3088drbwhwpm9lvln42yf23s";
    }
    else
    nixpkgs;
in
  import pkgSrc { overlays = [ haskellPackagesOverlay ]; }
