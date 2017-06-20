let
  config = {
    packageOverrides = pkgs: {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: {
          optparse-generic =
            haskellPackagesNew.callPackage ./nix/optparse-generic.nix { };
          fswait =
            haskellPackagesNew.callPackage ./default.nix { };
        };
      };
    };
  };

  pkgs =
    import <nixpkgs> { inherit config; system = "x86_64-linux"; };

in { fswait = pkgs.haskellPackages.fswait; }
