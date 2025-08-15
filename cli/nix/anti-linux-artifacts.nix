{ pkgs, node-project, version, project, ... }:
let
  anti = project.musl64.anti.components.exes.anti;
  tarball-derivation = pkgs.stdenv.mkDerivation rec {
    pname = "anti";
    inherit version;
    unpackPhase = ''
      mkdir -p $out/unpacked
      cp ${anti}/bin/anti $out/unpacked
      chmod -R +w $out/unpacked/*
    '';
    installPhase = ''
      tar -C $out/unpacked -czvf $out/$pname-$version-linux64.tar.gz .
      rm -rf $out/unpacked
    '';
  };
in { packages.linux64.tarball = tarball-derivation; }
