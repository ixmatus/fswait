args:

let
  # master as of 2021-09-28
  rev = "cbbf7a3de1978423e1da7cfc46e1d49c7d7154ed";

  sha256 = "0za544igvvhy2164fbkvysyrl50v67zm7z82h35iwnviaj82f41x";

  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256;
  };

in
import nixpkgs ({ config = { }; } // args)
