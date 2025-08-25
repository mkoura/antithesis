{ pkgs, project, node-project, version, rewrite-libs, ... }:

let
  inherit (pkgs) lib;
  anti = project.packages.anti;
  anti-oracle = project.packages.anti-oracle;
  tarball-derivation = pkgs.stdenv.mkDerivation {
    pname = "anti";
    inherit version;
    buildInputs = with pkgs.buildPackages; [ nix ];
    phases = [ "unpackPhase" "installPhase" ];
    unpackPhase = ''
      mkdir -p $out/unpacked
      cp ${anti}/bin/anti $out/unpacked
      cp ${anti-oracle}/bin/anti-oracle $out/unpacked
      ( cd $out/unpacked ;
        ${rewrite-libs}/bin/rewrite-libs . `ls -1 | grep -Fv .dylib`
        for a in *; do /usr/bin/codesign -f -s - $a; done
      )
      chmod -R +w $out/unpacked/*
    '';
    installPhase = ''
      tar -C $out/unpacked -czvf $out/$pname-$version-darwin64.tar.gz .
      rm -rf $out/unpacked
    '';
  };

in { packages.darwin64.tarball = tarball-derivation; }
