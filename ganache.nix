{ pkgs }:
let
  ganachecli = pkgs.stdenv.mkDerivation {
    name = "ganache-cli";
    phases = "buildPhase";
    buildInputs = [ pkgs.nodePackages_8_x.npm ];
    buildPhase = ''
      mkdir -p "$out/bin";
      mkdir -p "$out/tmp"
      export HOME=$TMP
      npm install --prefix "$out/tmp" ganache-cli
      mv "$out/tmp/node_modules/ganache-cli/build/cli.node.js" "$out/bin/cli.node.js"
      rm -r "$out/tmp"
    '';
  };
  runtestrpc = pkgs.writeScript "testrpc" ''
    ${pkgs.nodejs}/bin/node ${ganachecli}/bin/cli.node.js 1>/dev/null &
  '';
  killtestrpc = pkgs.writeScript "killtestrpc" ''
    ${pkgs.ps}/bin/ps aux|grep ganache|grep node|awk '{print $2}'|xargs kill -9
  '';
  withTestRpc = c: pkgs.writeScript "withTestRpc" ''
    ${runtestrpc}
    ${c}
    ${killtestrpc}
  '';
in
  { inherit withTestRpc; }
