{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, stdenv }:
      mkDerivation {
        pname = "vmandela-blog";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [ base ];

        buildDepends = [
          haskellPackages.pandoc
          haskellPackages.hakyll
          haskellPackages.raw-strings-qq
          pkgs.cabal-install
        ];

        license = stdenv.lib.licenses.unfree;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f { };

in

  if pkgs.lib.inNixShell then drv.env else drv
