{ mkDerivation, base, pure-txt, stdenv
}:
mkDerivation {
  pname = "pure-txt-search";
  version = "0.7.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base pure-txt
    ];
  homepage = "github.com/grumply/pure-txt-search";
  license = stdenv.lib.licenses.bsd3;
