{ nixpkgs ? import (builtins.fetchGit {
  name = "nixpkgs-unstable-2022-04-30";
  url = "https://github.com/nixos/nixpkgs/";
  ref = "refs/heads/nixos-22.05-small";
  rev = "7ae60dd7068478db5d936a3850b6df859aec21d0";
}) {}, compiler ? "ghc8107", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, composition, cond, containers
      , data-foldapp, edit-distance, extra, generic-lens, keys, lens, lib
      , matrices, megaparsec, parsec, parsec-numbers, parser-combinators
      , profunctors, recursion-schemes, search-algorithms, split, text
      , time, TypeCompose, universum, vector, cabal-install, haskell-language-server
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
        executableSystemDepends = [ haskell-language-server ];
        hydraPlatforms = lib.platforms.none;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
