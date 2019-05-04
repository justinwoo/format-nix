let
  nixpkgs = import <nixpkgs>;

  # some comment

in with nixpkgs; stdenv.mkDerivation {
  name = "something";

  buildInputs = [ ];
}
