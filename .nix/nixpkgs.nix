{ compiler ? null, nixpkgs ? null}:

let
  compilerVersion = if isNull compiler then "ghc884" else compiler;
  haskellPackagesOverlay = self: super: with super.haskell.lib; {
    haskellPackages = super.haskell.packages.${compilerVersion}.override {
      overrides = hself: hsuper: {
        gtfsschedule = hsuper.callPackage ./gtfsschedule.nix { };
        hprotoc = hsuper.callPackage ./hprotoc.nix { };
        protocol-buffers = hsuper.callPackage ./protocol-buffers.nix { };
      };
    };
  };
  pkgSrc =
    if isNull nixpkgs
    then
    # nixpkgs nixos-unstable - 2021-08-07
    builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/439b1605227b8adb1357b55ce8529d541abbe9eb.tar.gz";
      sha256 = "1jmlia9670lq89ic241zvif5q68qsqhqzm68pbdacxjmdrr0ahqf";
    }
    else
    nixpkgs;
in
import pkgSrc { overlays = [ haskellPackagesOverlay ]; }
