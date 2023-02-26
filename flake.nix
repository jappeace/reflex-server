# I used chatgpt to generate this template and then just
# modified to how I normally use these things.
{
  description = "My Haskell project";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-compat }:
    let
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
      hpkgs = pkgs.haskell.packages.ghc943.override {
        overrides = hold: hnew: {
          template-project = hnew.callCabal2nix "template-project" ./. { };
          reflex = hnew.callHackageDirect {
                pkg = "reflex";
                ver = "0.9.0.0";
                sha256 = "sha256-Kw7R7x8BL/kmOU1CeWgRYXO5q+JuvB+M6tzF8SfUakU=";
                } {};
          mmorph = hnew.callHackageDirect {
                pkg = "mmorph";
                ver = "1.1.5";
                sha256 = "sha256-C4V8WrDSo2RnPPFfoXATmjn++bBBa1ABS5ASeb2IQlQ=";
                } {};
          transformers-compat = hnew.callHackageDirect {
                pkg = "transformers-compat";
                ver = "0.6.6";
                sha256 = "sha256-xDCrZcvJKwtPjp8v86Ze1rBvzbJyEcfQqRHg6gxYHzk=";
                } {};
        };
      };
    in
    {
      defaultPackage.x86_64-linux =  hpkgs.template-project;
      devShell.x86_64-linux = hpkgs.shellFor {
        packages = ps : [ ps."template-project" ];
        withHoogle = true;

        buildInputs = [
          pkgs.ghcid
          pkgs.cabal-install
        ];
      };
    };
}
