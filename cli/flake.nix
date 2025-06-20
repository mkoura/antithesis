{
  description = "Antithesis CLI";

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.url = "github:NixOS/nixpkgs";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-utils.url = "github:hamishmack/flake-utils/hkm/nested-hydraJobs";
    iohkNix = {
      url = "github:input-output-hk/iohk-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    CHaP = {
      url = "github:intersectmbo/cardano-haskell-packages?ref=repo";
      flake = false;
    };
  };

  outputs = inputs@{ self, nixpkgs, flake-utils, haskellNix, CHaP, iohkNix, ... }:
    let
      lib = nixpkgs.lib;
      version = self.dirtyShortRev or self.shortRev;

      fix-blst = final: prev: {
        haskell-nix = prev.haskell-nix // {
          extraPkgconfigMappings = prev.haskell-nix.extraPkgconfigMappings // {
            # String pkgconfig-depends names are mapped to lists of Nixpkgs
            # package names
            "libblst" = [ "blst" ];
          };
        };
      };

      perSystem = system:
        let
          pkgs = import nixpkgs {
            overlays = [
              iohkNix.overlays.crypto # modified crypto libs
              iohkNix.overlays.cardano-lib
              haskellNix.overlay # some functions
              fix-blst
            ];
            inherit system;
          };

        in import ./project.nix {
          indexState = "2025-05-07T00:00:00Z";
          inherit CHaP;
          inherit version;
          inherit pkgs;
        };

    in flake-utils.lib.eachSystem
    [ "x86_64-linux" "aarch64-darwin" ] perSystem;
}
