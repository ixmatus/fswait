{ mkDerivation, base, bytestring, hinotify, optparse-applicative
, optparse-generic, semigroups, stdenv, stm, system-filepath, text
, time-units, turtle
}:
mkDerivation {
  pname = "fswait";
  version = "1.1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring hinotify optparse-applicative optparse-generic
    semigroups stm system-filepath text time-units turtle
  ];
  homepage = "https://github.com/ixmatus/fswait";
  description = "Wait and observe events on the filesystem for a path, with a timeout";
  license = stdenv.lib.licenses.bsd3;
}
