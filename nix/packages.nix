{ sources ? import ./sources.nix, compiler ? "ghc8104", system ? builtins.currentSystem }:

let
  nixpkgs = import sources.nixpkgs { inherit config; overlay = [overlay]; system=system;};
  gitignoreSource = (import sources."gitignore.nix" {}).gitignoreSource;
  #extra-deps = import ./extra-deps.nix {inherit system;};

  overlay = _: pkgs:
    {
      niv = (import sources.niv {}).niv;
    };
  config = { allowUnfree = true; allowBroken = true;
      packageOverrides = pkgs: rec {
        haskellPackages = pkgs.haskell.packages."${compiler}".override {
          overrides = self: super: (
            let
              sweph = super.callCabal2nix "swiss-ephemeris" (gitignoreSource ../.) {};
            in {
              # automatically create a derivation based on the cabal file present in ../.
              # override some cabal options:
              # https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/haskell-modules/lib.nix#L11
              # inspired by: https://jade.fyi/blog/nix-and-haskell/
              swiss-ephemeris = sweph;
              swiss-ephemeris-dist = pkgs.haskell.lib.sdistTarball sweph;
              swiss-ephemeris-docs = pkgs.haskell.lib.documentationTarball sweph;
            }
          );
        };
      };
  };
in nixpkgs
