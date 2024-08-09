args:

let
  # master as of 2024-08-09
  rev = "f345e5460b9406c545f2920d7b0116658b15d235";

  sha256 = "1gxjann17zr8y5l35kv7b87rv61r3kzj1r4x7krb30wz4fw52xiv";

  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256;
  };

in
import nixpkgs ({ config = { }; } // args)
