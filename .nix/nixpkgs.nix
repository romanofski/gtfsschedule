{ compiler ? null, nixpkgs ? null}:

let
  compilerVersion = if isNull compiler then "ghc884" else compiler;
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
    # nixpkgs nixos-unstable - 2021-01-06
    builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/d2b489b24ae30278ad31e06801c3853b0edbbd9d.tar.gz";
      sha256 = "1c9k3589fyn510by0dvb8c4r8p8f5vrlabf2q342g64yxsar4h2m";
    }
    else
    nixpkgs;
in
import pkgSrc { overlays = [ haskellPackagesOverlay ]; }
