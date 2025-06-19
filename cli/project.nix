{ CHaP, system, version, indexState, haskell-nix, pkgs, ... }:

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
}
