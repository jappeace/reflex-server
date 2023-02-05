import ./pin.nix {
  config = {

    packageOverrides = pkgs: {
        haskell = pkgs.lib.recursiveUpdate pkgs.haskell {
        packageOverrides = hpNew: hpOld: {
            reflex-server = hpNew.callPackage ../default.nix {};
            };
        };
    };
  };
}
