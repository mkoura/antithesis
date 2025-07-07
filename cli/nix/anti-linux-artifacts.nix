{ pkgs, node-project, version, project, ... }:
let
  cardano-cli = node-project.projectCross.musl64.pkgs.cardano-cli;
  cardano-address =
    project.musl64.cardano-addresses.components.exes.cardano-address;
  bech32 = project.musl64.bech32.components.exes.bech32;
  anti = project.musl64.anti.components.exes.anti;
  tarball-derivation = pkgs.stdenv.mkDerivation rec {
    pname = "anti-linux-tarball";
    inherit version;
    unpackPhase = ''
      mkdir -p $out/unpacked
      cp ${anti}/bin/anti $out/unpacked
      cp ${bech32}/bin/bech32 $out/unpacked
      cp ${cardano-address}/bin/cardano-address $out/unpacked
      cp ${cardano-cli}/bin/cardano-cli $out/unpacked
      chmod -R +w $out/unpacked/*
    '';
    installPhase = ''
      tar -C $out/unpacked -czvf $out/$pname-$version-linux64.tar.gz .
      rm -rf $out/unpacked
    '';
  };
in {
  packages.linux64.tarball = tarball-derivation;
}
