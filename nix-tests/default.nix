let pkgs = import <nixpkgs> {};
in import ./vmTest.nix {
  inherit pkgs;
  test = {
    testScript = ''
      startAll;
      $master->succeed("echo wow");
    '';
    name = "ivy-deployment-tests";
    nodes = {
      master = { config, nodes, ... }: {
        virtualisation.vlans = [1]; 
        virtualisation.memorySize = 128; 
        networking = { enableIPv6 = false; firewall.enable = false; };
      };
    };
  };   
}
