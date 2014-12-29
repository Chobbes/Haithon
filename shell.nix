{ pkgs ? (import <nixpkgs> {})
, haskellPackages ? pkgs.haskellPackages_ghc783
}:

haskellPackages.cabal.mkDerivation (self: {
  pname = "Haithon";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = false;
  buildDepends = with haskellPackages; [ parsec indents ];
  buildTools = with haskellPackages; [ cabalInstall ];
  meta = {
    homepage = "https://github.com/Chobbes/Haithon";
    description = "A Haskell parser for a subset of the Python language. The intention of this project is two-fold! Firstly, I wish to try out Parsec's handling of indentation sensitive languages, and secondly I want to experiment with using this as some course material!";
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
})
