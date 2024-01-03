{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    fenix.url = "github:nix-community/fenix/monthly";
    fenix.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, flake-utils, fenix }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        fenixPkgs = fenix.packages.${system};
      in {
        devShells.default = pkgs.mkShell {
          packages = [
            (fenixPkgs.complete.withComponents [ "cargo" "rustc" "rustfmt" "clippy" ])
            # fenixPkgs.rust-analyzer
          ];
        };
      });
}
