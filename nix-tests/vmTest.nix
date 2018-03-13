{ pkgs, test }:

let makeTest = import (pkgs.path + "/nixos/tests/make-test.nix");
in makeTest test {}
