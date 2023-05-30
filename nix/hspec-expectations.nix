{ mkDerivation, base, call-stack, HUnit, lib, nanospec }:
mkDerivation {
  pname = "hspec-expectations";
  version = "0.8.3";
  sha256 = "71bf0a0ae521fbb25bc105d0b9e1c72e825881a468f0c051f28a66e717d02172";
  libraryHaskellDepends = [ base call-stack HUnit ];
  testHaskellDepends = [ base call-stack HUnit nanospec ];
  homepage = "https://github.com/hspec/hspec-expectations#readme";
  description = "Catchy combinators for HUnit";
  license = lib.licenses.mit;
}
