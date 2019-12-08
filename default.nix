{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc881" }:
let
  itn = builtins.fetchTarball https://github.com/HaskellEmbedded/ivory-tower-nix/archive/master.tar.gz;
  overlays = import "${itn}/overlay.nix" compiler;
  pkgs = import "${itn}/nixpkgs.nix" { inherit overlays; };
  src = pkgs.nix-gitignore.gitignoreSource [] ./.;
in
  pkgs.myHaskellPackages.callCabal2nix "monstick-firmware" "${src}" {}
