{
  description = "launch";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        bugsnag-hs = pkgs.haskellPackages.callCabal2nix "junit-xml" ./. { };
      in {
        defaultPackage = bugsnag-hs;
        devShell = pkgs.haskellPackages.shellFor {
          packages = p: [ bugsnag-hs ];
          buildInputs = [
            pkgs.cabal-install
            pkgs.haskellPackages.ghcid
            pkgs.haskellPackages.hpack
            pkgs.ormolu
          ];
        };
      });
}
