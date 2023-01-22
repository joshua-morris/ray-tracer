{ mkDerivation, base, zlib}:
mkDerivation {
  pname = "rays";
  version = "0.0.0.9";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base
  ];
}
