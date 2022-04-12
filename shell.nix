{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, composition, cond, containers
      , data-foldapp, edit-distance, extra, generic-lens, keys, lens, lib
      , matrices, megaparsec, parsec, parsec-numbers, parser-combinators
      , profunctors, recursion-schemes, search-algorithms, split, text
      , time, TypeCompose, universum, vector, cabal-install
      }:
      mkDerivation {
        pname = "advent";
        version = "1.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          base composition cond containers data-foldapp edit-distance extra
          generic-lens keys lens matrices megaparsec parsec parsec-numbers
          parser-combinators profunctors recursion-schemes search-algorithms
          split text time TypeCompose universum vector cabal-install
        ];
        license = "unknown";
        hydraPlatforms = lib.platforms.none;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
