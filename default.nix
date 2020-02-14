{ mkDerivation, base, pure-core, pure-default, pure-lifted, pure-txt, text, bytestring, containers, stdenv
}:
mkDerivation {
  pname = "pure-txt-search";
  version = "0.8.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base pure-core pure-default pure-lifted pure-txt text bytestring containers
    ];
  homepage = "github.com/grumply/pure-txt-search";
  license = stdenv.lib.licenses.bsd3;
}
