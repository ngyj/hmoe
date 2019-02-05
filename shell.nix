with import <nixpkgs> {};

let
  ghc = haskell.packages.ghc844.ghcWithPackages (pkgs: with pkgs; [ zlib ]);
in
  stdenv.mkDerivation {
    name = "haskell.moe";
    src = ./.;
    buildInputs = [
      elmPackages.elm
      ghc
    ];
  }
