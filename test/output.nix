(expression (comment) (function (formals (formal (identifier) (app (app (identifier) (spath)) (attrset)))) (let (bind (attrpath (identifier)) (select (identifier) (attrpath (identifier) (identifier) (identifier) (identifier)))) (bind (attrpath (identifier)) (function (identifier) (if (select (identifier) (attrpath (identifier) (identifier))) (string) (indented_string (interpolation (identifier)) (interpolation (identifier)))))) (app (select (identifier) (attrpath (identifier) (identifier))) (rec_attrset (bind (attrpath (identifier)) (string)) (bind (attrpath (identifier)) (string)) (bind (attrpath (identifier)) (if (select (identifier) (attrpath (identifier) (identifier))) (app (select (identifier) (attrpath (identifier))) (attrset (bind (attrpath (identifier)) (string)) (bind (attrpath (identifier)) (string)))) (app (select (identifier) (attrpath (identifier))) (attrset (bind (attrpath (identifier)) (string)) (bind (attrpath (identifier)) (string)))))) (bind (attrpath (identifier)) (list (select (identifier) (attrpath (identifier))) (select (identifier) (attrpath (identifier))) (select (identifier) (attrpath (identifier))))) (bind (attrpath (identifier)) (app (select (identifier) (attrpath (identifier) (identifier))) (identifier))) (bind (attrpath (identifier)) (identifier)) (bind (attrpath (identifier)) (indented_string (interpolation (app (identifier) (identifier))))))))))
# https://github.com/justinwoo/easy-purescript-nix/blob/7255d015b80d28c7c6db655dda215535cb2d4b41/purs.nix

{ pkgs ? import <nixpkgs> {} }:

let
  dynamic-linker = pkgs.stdenv.cc.bintools.dynamicLinker;

  patchelf = libPath: if pkgs.stdenv.isDarwin
    then ""
    else ''
      chmod u+w $PURS
      patchelf --interpreter ${dynamic-linker} --set-rpath ${libPath} $PURS
      chmod u-w $PURS
    '';

in pkgs.stdenv.mkDerivation rec {
  name = "purs-simple";

  version = "v0.12.3";

  src = if pkgs.stdenv.isDarwin
    then pkgs.fetchurl {
      url = "https://github.com/purescript/purescript/releases/download/v0.12.3/macos.tar.gz";
      sha256 = "1f916gv4fz571l4jvr15xjnsvjyy4nljv2ii9njwlm7k6yr5m0qn";
    }
    else pkgs.fetchurl {
      url = "https://github.com/purescript/purescript/releases/download/v0.12.3/linux64.tar.gz";
      sha256 = "1fad862a2sv4njxbbcfzibbi585m6is3ywb94nmjl8ax254baj3i";
    };

  buildInputs = [ pkgs.zlib pkgs.gmp pkgs.ncurses5 ];

  libPath = pkgs.lib.makeLibraryPath buildInputs;

  dontStrip = true;

  installPhase = ''
    mkdir -p $out/bin
    PURS="$out/bin/purs"

    install -D -m555 -T purs $PURS
    ${patchelf libPath}

    mkdir -p $out/etc/bash_completion.d/
    $PURS --bash-completion-script $PURS > $out/etc/bash_completion.d/purs-completion.bash
  '';
}


(expression (app (app (identifier) (path)) (attrset)))
import ./build.nix {}


(expression (let (bind (attrpath (identifier)) (integer)) (bind (attrpath (identifier)) (integer)) (binary (unary (identifier)) (identifier))))
let
  a = 123;

  b = 123;

in -a + b


(expression (function (formals (formal (identifier) (string)) (formal (identifier) (string))) (binary (identifier) (identifier))))
{ foo ? "foo", bar ? "bar" }:

foo + bar


(expression (function (formals (formal (identifier)) (formal (identifier)) (formal (identifier)) (formal (identifier)) (formal (identifier)) (formal (identifier)) (formal (identifier)) (formal (identifier)) (formal (identifier)) (formal (identifier)) (formal (identifier)) (formal (identifier)) (formal (identifier)) (formal (identifier)) (formal (identifier)) (formal (identifier)) (formal (identifier)) (formal (identifier)) (formal (identifier)) (formal (identifier)) (formal (identifier)) (formal (identifier))) (integer)))
{
  apple,
  bpple,
  cpple,
  dpple,
  epple,
  fpple,
  gpple,
  hpple,
  ipple,
  jpple,
  kpple,
  lpple,
  mpple,
  npple,
  opple,
  ppple,
  qpple,
  rpple,
  spple,
  tpple,
  upple,
  vpple
}:

1


(expression (attrset (inherit (parenthesized (identifier)) (attrs (identifier) (identifier) (identifier) (identifier))) (bind (attrpath (identifier)) (with (parenthesized (identifier)) (integer))) (inherit (parenthesized (identifier)) (attrs (identifier) (identifier) (identifier) (identifier) (identifier) (identifier) (identifier) (identifier) (identifier) (identifier) (identifier) (identifier) (identifier) (identifier) (identifier) (identifier) (identifier) (identifier) (identifier) (identifier) (identifier) (identifier)))))
{
  inherit (x) a b c d;

  z = with (a); 1;

  inherit (x)
    apple
    bpple
    cpple
    dpple
    epple
    fpple
    gpple
    hpple
    ipple
    jpple
    kpple
    lpple
    mpple
    npple
    opple
    ppple
    qpple
    rpple
    spple
    tpple
    upple
    vpple;
}


(expression (comment) (function (formals (formal (identifier) (app (app (identifier) (spath)) (attrset)))) (app (app (identifier) (parenthesized (app (select (identifier) (attrpath (identifier))) (attrset (bind (attrpath (identifier)) (string)) (bind (attrpath (identifier)) (string)) (bind (attrpath (identifier)) (string)) (bind (attrpath (identifier)) (string)))))) (attrset (inherit (attrs (identifier)))))))
# https://github.com/justinwoo/easy-purescript-nix/blob/7255d015b80d28c7c6db655dda215535cb2d4b41/psc-package2nix.nix

{ pkgs ? import <nixpkgs> {} }:

import (pkgs.fetchFromGitHub {
  owner = "justinwoo";
  repo = "psc-package2nix";
  rev = "b4d6a834ac124440a503f0510b8a9de95532b16c";
  sha256 = "0g9fq4j472bcr1x5na6mzr3av95xhvdmnlns1ncvsl4kqa8ix2zr";
}) {
  inherit pkgs;
}


(expression (attrset (bind (attrpath (identifier)) (list (string) (string) (string) (string) (string) (string) (string) (string) (string) (string) (string) (string) (string) (string) (string) (string) (string) (string) (string) (string) (string) (string))) (bind (attrpath (identifier)) (list (integer) (integer) (integer) (integer) (integer)))))
{
  a = [
    "apple"
    "bpple"
    "cpple"
    "dpple"
    "epple"
    "fpple"
    "gpple"
    "hpple"
    "ipple"
    "jpple"
    "kpple"
    "lpple"
    "mpple"
    "npple"
    "opple"
    "ppple"
    "qpple"
    "rpple"
    "spple"
    "tpple"
    "upple"
    "vpple"
  ];

  b = [ 1 2 3 4 5 ];
}


(expression (let (bind (attrpath (identifier)) (app (identifier) (parenthesized (app (select (identifier) (attrpath (identifier))) (uri))))) (bind (attrpath (identifier)) (app (app (identifier) (spath)) (attrset (bind (attrpath (identifier)) (list (identifier)))))) (with (identifier) (app (select (identifier) (attrpath (identifier))) (attrset (bind (attrpath (identifier)) (string)) (bind (attrpath (identifier)) (list (select (identifier) (attrpath (identifier) (identifier) (identifier) (identifier))))))))))
let
  moz_overlay = import (builtins.fetchTarball https://github.com/mozilla/nixpkgs-mozilla/archive/master.tar.gz);

  nixpkgs = import <nixpkgs> {
    overlays = [ moz_overlay ];
  };

in with nixpkgs; stdenv.mkDerivation {
  name = "moz_overlay_shell";

  buildInputs = [ nixpkgs.latest.rustChannels.nightly.rust ];
}


(expression (let (bind (attrpath (identifier)) (app (identifier) (spath))) (comment) (with (identifier) (app (select (identifier) (attrpath (identifier))) (attrset (bind (attrpath (identifier)) (string)) (bind (attrpath (identifier)) (list)))))))
let
  nixpkgs = import <nixpkgs>;

  # some comment

in with nixpkgs; stdenv.mkDerivation {
  name = "something";

  buildInputs = [ ];
}


(expression (function (formals (formal (identifier)) (formal (identifier)) (ellipses)) (attrset (bind (attrpath (identifier) (identifier)) (string)))))
{ config, pkgs, ... }:

{
  system.stateVersion = "19.09";
}
