{ compiler ? null, nixpkgs ? null }:

with (import .nix/nixpkgs.nix { inherit compiler nixpkgs; });

let
  envPackages = self: [ self.gtfsschedule ];
  env = haskellPackages.ghcWithPackages envPackages;
  nativeBuildTools = with pkgs.haskellPackages; [
    cabal-install
    cabal2nix
    ghcid
    hindent
    hlint
  ];
in
    if pkgs.lib.inNixShell
    then haskellPackages.shellFor {
      withHoogle = true;
      packages = haskellPackages: [ haskellPackages.gtfsschedule ];
      nativeBuildInputs = haskellPackages.gtfsschedule.env.nativeBuildInputs ++ nativeBuildTools;
    }
    else haskellPackages.gtfsschedule
