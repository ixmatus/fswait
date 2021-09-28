let
  pkgs = import ./nixpkgs.nix { };

  fswait = import ./default.nix;

in
fswait.env.overrideAttrs (prev: {
  buildInputs = (prev.buildInputs or [ ]) ++ [
    pkgs.cabal-install
  ];
})
