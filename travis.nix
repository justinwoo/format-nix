{ pkgs ? import <nixpkgs> {} }:

let
  easy-ps = import (pkgs.fetchFromGitHub {
    owner = "justinwoo";

    repo = "easy-purescript-nix";

    rev = "e3501174123c884d5878b42a48e361aa113cdead";

    sha256 = "1c28ml262qnh7c1rsz86gnmrj9k9gi35rxh2mx1mwr2ac5dl213a";
  });

in pkgs.stdenv.mkDerivation {
  name = "travis-shell";

  buildInputs = [
    pkgs.nodejs
    easy-ps.purs
    easy-ps.spago
  ];
}
