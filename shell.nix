with import <nixpkgs> {};

haskell.lib.buildStackProject {
  name = "none";
  inherit ghc;
  buildInputs = [
    git
    zlib
  ];
}
