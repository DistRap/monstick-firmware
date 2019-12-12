{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc881" }:
let
  default = (import ./default.nix { inherit nixpkgs compiler; });
in
nixpkgs.stdenv.mkDerivation {
  name = "monstick-shell";

  buildInputs = [
    default.env.nativeBuildInputs
    nixpkgs.gnumake
    nixpkgs.gcc-arm-embedded
  ];

  shellHook = ''
    echo "Monstick shell"
  '';
}
