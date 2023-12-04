args:

let
  # master as of 2023-12-04
  rev = "fe01686ae3d5c3f26998a26bfabbedc87023480d";

  sha256 = "sha256:0lygv93plycpchd0z0k18fqby1g9yz7m6g8rd0kfmbzi75zra9kh";

  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256;
  };

in
import nixpkgs ({ config = { }; } // args)
