{ pkgs ? import <nixpkgs> {} }:

let
  easy-ps = import (pkgs.fetchFromGitHub {
    owner = "justinwoo";

    repo = "easy-purescript-nix";

    rev = "647b28018c348832e47a0f17aa0994f0e4c1e5b5";

    sha256 = "0r6jvq64fbfyw8acl7xcfm0wsq1n52jbx6a2s31fdgk5hg30ch8f";
  });

in pkgs.stdenv.mkDerivation {
  name = "travis-shell";

  buildInputs = [ pkgs.nodejs easy-ps.psc-package-simple easy-ps.purs ];
}
