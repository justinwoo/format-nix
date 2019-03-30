{pkgs ? import <nixpkgs> {}}:

let
  inherit (pkgs) lib;
  nodejs = pkgs.nodejs-8_x;
  nodeEnv = import ./node-env.nix {
    inherit (pkgs) stdenv python2 utillinux runCommand writeTextFile;
    inherit nodejs;
    libtool = if pkgs.stdenv.isDarwin then pkgs.darwin.cctools else null;
  };
in
  ((import ./node-packages.nix {
    inherit (pkgs) fetchurl fetchgit;
    inherit nodeEnv;
  }).package).overrideAttrs (o: rec {
    name = "format-nix";
    version = "git";
    meta = {
      description = "A simple formatter for Nix";
      homepage = https://github.com/justinwoo/format-nix;
      license = lib.licenses.mit;
      platforms = lib.platforms.linux ++ lib.platforms.darwin;
    };

  })
