{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:
let
  itnSrc = nixpkgs.fetchFromGitHub {
    owner = "HaskellEmbedded";
    repo = "ivory-tower-nix";
    rev = "860fb41b4d489e536634fde23adb808c8cf337f1";
    sha256 = "043xwy3i1d9m057fyd3n5bmbywqhd8hccdxby6div3hpsbhf966g";
  };

  itn = import itnSrc { inherit compiler; };
  src = itn.pkgs.nix-gitignore.gitignoreSource [] ./.;
in
  itn // rec {
    monstick-firmware =
      itn.ivorypkgs.callCabal2nix "monstick-firmware" "${src}" {};

    shell = itn.mkShell monstick-firmware;
  }

