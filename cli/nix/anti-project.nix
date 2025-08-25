{ CHaP, indexState, pkgs, cardano-cli, ... }:

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
      hlint = { index-state = indexState; };
    };
    withHoogle = true;
    buildInputs = [
      pkgs.gitAndTools.git
      pkgs.just
      cardano-cli
      project.hsPkgs.cardano-addresses.components.exes.cardano-address
      project.hsPkgs.bech32.components.exes.bech32
      pkgs.nixfmt-classic

    ];
    shellHook = ''
      echo "Entering shell for anti CLI development"
    '';
  };

  fullyStaticOptions = { pkgs, ... }:
    let libs = with pkgs; [ zlib openssl libffi gmp6 pkgs.secp256k1 ];
    in {
      enableShared = false;
      enableStatic = true;
      configureFlags = map (l: "--ghc-option=-optl=-L${l}/lib") (libs);
    };
  musl = { pkgs, ... }: {
    packages.anti.components.exes.anti = (fullyStaticOptions { inherit pkgs; });
    doHaddock = false;
  };
  mkProject = ctx@{ lib, pkgs, ... }: {
    name = "anti";
    src = ./..;
    compiler-nix-name = "ghc984";
    shell = shell { inherit pkgs; };
    modules = [ libOverlay ];
    inputMap = { "https://chap.intersectmbo.org/" = CHaP; };
  };
  project = pkgs.haskell-nix.cabalProject' mkProject;

in {
  devShells.default = project.shell;
  inherit project;
  packages.anti = project.hsPkgs.anti.components.exes.anti;
  packages.anti-oracle = project.hsPkgs.anti.components.exes.anti-oracle;
  packages.bech32 = project.hsPkgs.bech32.components.exes.bech32;
  packages.cardano-address =
    project.hsPkgs.cardano-addresses.components.exes.cardano-address;
  musl64 = project.projectCross.musl64.hsPkgs;
}
