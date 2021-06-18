let
  pkgs = import ./packages.nix {};
in
  { 
    swiss-ephemeris = pkgs.haskellPackages.swiss-ephemeris;
    dist = pkgs.haskellPackages.swiss-ephemeris-dist;    
    doc = pkgs.haskellPackages.swiss-ephemeris-docs;    
  }
