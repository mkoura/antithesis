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
          libOverlay = { lib, pkgs, ... }: {
            # Use our forked libsodium from iohk-nix crypto overlay.
            packages.plutus-tx.components.library.pkgconfig =
              lib.mkForce [[ pkgs.libsodium-vrf pkgs.secp256k1 ]];
            packages.byron-spec-ledger.components.library.pkgconfig =
              lib.mkForce [[ pkgs.libsodium-vrf pkgs.secp256k1 ]];
            packages.cardano-crypto-praos.components.library.pkgconfig =
              lib.mkForce [[ pkgs.libsodium-vrf pkgs.secp256k1 ]];
            packages.cardano-crypto-class.components.library.pkgconfig =
              lib.mkForce [[ pkgs.libsodium-vrf pkgs.secp256k1 pkgs.libblst ]];
          };

          pkgs = import nixpkgs {
            overlays = [
              iohkNix.overlays.crypto # modified crypto libs
              iohkNix.overlays.cardano-lib
              haskellNix.overlay # some functions
              fix-blst
            ];
            inherit system;
          };
          indexState = "2025-05-07T00:00:00Z";

          shell = { pkgs, ... }: {
            tools = {
              cabal = { index-state = indexState; };
              cabal-fmt = { index-state = indexState; };
              haskell-language-server = { index-state = indexState; };
              hoogle = { index-state = indexState; };
              fourmolu = { index-state = indexState; };
            };
            withHoogle = true;
            buildInputs = [
              pkgs.just
              pkgs.gitAndTools.git
              pkgs.haskellPackages.ghcid
              pkgs.haskellPackages.hlint
            ];
            shellHook = ''
              echo "Entering shell for anti CLI development"
            '';
          };
          mkProject = ctx@{ lib, pkgs, ... }: {
            name = "anti";
            src = ./.;
            compiler-nix-name = "ghc984";
            shell = shell { inherit pkgs; };
            modules = [ libOverlay ];
            inputMap = { "https://chap.intersectmbo.org/" = CHaP; };
          };
          project = pkgs.haskell-nix.cabalProject' mkProject;

        in {
          devShells.default = project.shell;
          inherit project;
          inherit version;
          packages.anti = project.hsPkgs.anti.components.exes.anti;
        };
    in flake-utils.lib.eachSystem [ "x86_64-linux" "aarch64-darwin" ] perSystem;
}
