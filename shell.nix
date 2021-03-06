with import <nixpkgs> {};

let
  ghc = haskell.packages.ghc844.ghcWithPackages (pkgs: with pkgs; [ zlib gd ]);
in
  stdenv.mkDerivation {
    name = "hmoe";
    src = ./.;
    buildInputs = [
      elmPackages.elm
      ghc
    ];
  }
