let
  pkgs = import <nixpkgs> {};
  compiler = pkgs.haskell.packages.ghc822;
  ivy = import ./default.nix;
in
  {
    env = ivy.env;
  }
