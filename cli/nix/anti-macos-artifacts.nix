{ pkgs, project, node-project, version, rewrite-libs, ... }:

let
  inherit (pkgs) lib;
  cardano-cli = node-project.pkgs.cardano-cli;
  cardano-address = project.packages.cardano-address;
  bech32 = project.packages.bech32;
  anti = project.packages.anti;
  tarball-derivation = pkgs.stdenv.mkDerivation {
    pname = "anti-macos-tarball";
    inherit version;
    buildInputs = with pkgs.buildPackages; [ nix ];
    phases = [ "unpackPhase" "installPhase" ];
    unpackPhase = ''
      mkdir -p $out/unpacked
      cp ${anti}/bin/anti $out/unpacked
      cp ${bech32}/bin/bech32 $out/unpacked
      cp ${cardano-address}/bin/cardano-address $out/unpacked
      cp ${cardano-cli}/bin/cardano-cli $out/unpacked
      ( cd $out/unpacked ;
        ${rewrite-libs}/bin/rewrite-libs . `ls -1 | grep -Fv .dylib`
        for a in *; do /usr/bin/codesign -f -s - $a; done
      )
      chmod -R +w $out/unpacked/*
    '';
    installPhase = ''
      tar -C $out/unpacked -czvf $out/$pname-$version-macos.tar.gz .
      rm -rf $out/unpacked
    '';
  };

in { packages.macos64.tarball = tarball-derivation; }
