with import <nixpkgs> {};

let
  ghc = haskell.packages.ghc865.ghcWithPackages (pkgs: with pkgs; [ zlib gd ]);
in
  stdenv.mkDerivation {
    name = "hmoe";
    src = ./.;
    buildInputs = [
      elmPackages.elm
      ghc
    ];
  }
