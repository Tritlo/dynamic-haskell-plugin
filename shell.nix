# This imports the nix package collection,
# so we can access the `pkgs` and `stdenv` variables
with import <nixpkgs> {};



# Make a new "derivation" that represents our shell
stdenv.mkDerivation {
  name = "ghc-8.10";

  # The packages in the `buildInputs` list will be added to the PATH in our shell
  buildInputs = [
    # cowsay is an arbitary package
    # see https://nixos.org/nixos/packages.html to search for more
    zlib haskell.compiler.ghc884
  ];
}
