{ allowUnfree = true;
  packageOverrides = pkgs: {
    haskellPackages = pkgs.haskellPackages.override {
      overrides = haskellPackagesNew: haskellPackagesOld: {
        optparse-generic =
          haskellPackagesNew.callPackage ./nix/optparse-generic.nix { };

        fswait = haskellPackagesNew.callPackage ./default.nix { };
      };
    };
  };
}
