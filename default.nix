let pkgs = import ./nixpkgs.nix;

in pkgs.haskellPackages.callCabal2nix "junit-xml" ./. { }
