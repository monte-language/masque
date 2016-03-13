{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, binary, bound, bytestring, containers
      , data-binary-ieee754, either, free, GenericPretty, hlint, lens, mtl
      , network, prelude-extras, semigroups, stdenv, transformers
      }:
      mkDerivation {
        pname = "masque";
        version = "15.4.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          base binary bound bytestring containers data-binary-ieee754 either
          free GenericPretty hlint lens mtl network prelude-extras semigroups
          transformers
        ];
        homepage = "http://github.com/monte-language/masque";
        license = stdenv.lib.licenses.gpl3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
