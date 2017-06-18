{ mkDerivation, base, hinotify, neat-interpolation
, optparse-applicative, optparse-generic, stdenv, stm
, system-filepath, text, time-units, turtle
}:
mkDerivation {
  pname = "fswait";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base hinotify neat-interpolation optparse-applicative
    optparse-generic stm system-filepath text time-units turtle
  ];
  homepage = "https://github.com/ixmatus/fswait";
  description = "Wait and observe events on the filesystem for a path, with a timeout";
  license = stdenv.lib.licenses.bsd3;
}
