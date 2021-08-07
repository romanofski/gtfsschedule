{ compiler ? null, nixpkgs ? null}:

let
  compilerVersion = if isNull compiler then "ghc884" else compiler;
  haskellPackagesOverlay = self: super: with super.haskell.lib; {
    haskellPackages = super.haskell.packages.${compilerVersion}.override {
      overrides = hself: hsuper: {
        gtfsschedule = hsuper.callPackage ./gtfsschedule.nix { };
        hprotoc = hsuper.callPackage ./hprotoc.nix { };
      };
    };
  };
  pkgSrc =
    if isNull nixpkgs
    then
    # nixpkgs nixos-unstable - 2021-01-06
    builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/f6b5bfdb470d60a876992749d0d708ed7b6b56ca.tar.gz";
      sha256 = "1rfsyz5axf2f7sc14wdm8dmb164xanbw7rcw6w127s0n6la17kq2";
    }
    else
    nixpkgs;
in
import pkgSrc { overlays = [ haskellPackagesOverlay ]; }
