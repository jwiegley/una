{
  description = "Universal unarchiving software";

  inputs = {
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    haskellNix.url = "github:input-output-hk/haskell.nix";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs {
        inherit system overlays;
        inherit (haskellNix) config;
      };
      flake = pkgs.una.flake {
      };
      overlays = [ haskellNix.overlay
        (final: prev: {
          una =
            final.haskell-nix.project' {
              src = ./.;
              supportHpack = true;
              compiler-nix-name = "ghc910";
              shell.tools = {
                cabal = {};
                haskell-language-server = {};
                # hlint = {};
              };
              shell.buildInputs = with pkgs; [
                pkg-config
              ];
            };
        })
      ];
    in {
      packages.default = flake.packages."una:exe:una";
      devShells.default = flake.devShells.default // {
        withHoogle = true;
      };
    });
}
