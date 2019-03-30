{ pkgs ? import <nixpkgs> {} }:

let
  psc-package2nix = import (pkgs.fetchFromGitHub {
    owner = "justinwoo";

    repo = "psc-package2nix";

    rev = "b4d6a834ac124440a503f0510b8a9de95532b16c";

    sha256 = "0g9fq4j472bcr1x5na6mzr3av95xhvdmnlns1ncvsl4kqa8ix2zr";
  }) {
    inherit pkgs;
  };

  easy-ps = import (pkgs.fetchFromGitHub {
    owner = "justinwoo";

    repo = "easy-purescript-nix";

    rev = "7255d015b80d28c7c6db655dda215535cb2d4b41";

    sha256 = "1wvg9733lyadz9dpq3f2a516xqfyfmxihhnvd1pgcs1hwxb3p35k";
  });

  mkCompileDirect = import "${psc-package2nix.src}/nix/mkCompileDirect.nix";

  packages = import ./packages.nix {
    inherit pkgs;
  };

  output = mkCompileDirect {
    inherit packages;

    inherit (easy-ps) purs;

    src = ./.;
  };

in pkgs.stdenv.mkDerivation {
  name = "format-nix-js-bundle";

  src = output;

  installPhase = ''
    ${easy-ps.purs}/bin/purs bundle 'output/**/*.js' --main Main -m Main -o bundle.js
    mkdir -p $out
    mv bundle.js $out
  '';
}
