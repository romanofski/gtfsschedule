# Building gtfsschedule
#
# You should be able to simply invoke:
#
# $ nix-build
#
# or, to be explicit:
#
# $ nix-build default.nix
#
# in gtfsschedule's root directory in order to build gtfsschedule. You'll find the binary under:
#
# $ ls result/bin/gtfsschedule
#
# if the build was successful.
#
#
# Choosing a different compiler than the default
#
# In order to choose a different compiler, invoke nix build like so (escaping
# the quotes is needed, since we're passing a string literal):
#
# $ nix-build --arg compiler \"ghc442\"
#
# Use as a development environment
#
# $ nix-shell default.nix
#
{ compiler ? null, nixpkgs ? null, with-icu ? false }:

with (import .nix/nixpkgs.nix { inherit compiler nixpkgs; });

let
  envPackages = self: [ self.gtfsschedule ];
  env = haskellPackages.ghcWithPackages envPackages;
  nativeBuildTools = with pkgs.haskellPackages; [
    cabal-install
    cabal2nix
    ghcid
    hlint
    haskell-language-server
    ormolu
    hie-bios
    pkgs.gnumake
    pkgs.asciidoctor
    pkgs.python3Packages.pygments
  ];
in
if pkgs.lib.inNixShell
then haskellPackages.shellFor {
  withHoogle = true;
  packages = haskellPackages: [ haskellPackages.gtfsschedule ];
  nativeBuildInputs = haskellPackages.gtfsschedule.env.nativeBuildInputs ++ nativeBuildTools;
}
else {
  gtfsschedule = pkgs.stdenv.mkDerivation {
    name = "gtfsschedule-with-packages-${env.version}";
    nativeBuildInputs = [ pkgs.makeWrapper ];
    buildCommand = ''
      mkdir -p $out/bin
      makeWrapper ${env}/bin/gtfsschedule $out/bin/gtfsschedule \
      --set NIX_GHC "${env}/bin/ghc"
    '';
    preferLocalBuild = true;
    allowSubstitutes = false;
  };
}
