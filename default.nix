let
  pkgs = import <nixpkgs> {};
  compiler = pkgs.haskell.packages.ghc822;
  ivy = compiler.callPackage ./ivy {};
  stdlib = ./ivy/stdlib.ivy;
  runIvyMode = mode: pkgs.writeScript "ivy-${mode}" ''
    ${ivy}/bin/ivy -m ${mode} -i ${stdlib}
  '';
  ganache = import ./ganache.nix { inherit pkgs; };
  deployment-js = ./ivy/deployment/deployment.js;
  runTests = import ./test.nix { inherit pkgs deployment-js; };
in
  {
    ast      = runIvyMode "ast";
    bytecode = runIvyMode "bytecode";
    asm      = runIvyMode "asm";
    disasm   = runIvyMode "disasm";
    deploy   = runIvyMode "deploy";
    test     = pkgs.writeScript "test" ''
      ${ganache.withTestRpc (runTests ivy)}
    '';
  }
