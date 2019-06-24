(expression (comment) (function (formals (formal (identifier) (app (app (identifier) (spath)) (attrset)))) (let (bind (attrpath (identifier)) (select (identifier) (attrpath (identifier) (identifier) (identifier) (identifier)))) (bind (attrpath (identifier)) (function (identifier) (if (select (identifier) (attrpath (identifier) (identifier))) (string) (indented_string (interpolation (identifier)) (interpolation (identifier)))))) (app (select (identifier) (attrpath (identifier) (identifier))) (rec_attrset (bind (attrpath (identifier)) (string)) (bind (attrpath (identifier)) (string)) (bind (attrpath (identifier)) (if (select (identifier) (attrpath (identifier) (identifier))) (app (select (identifier) (attrpath (identifier))) (attrset (bind (attrpath (identifier)) (string)) (bind (attrpath (identifier)) (string)))) (app (select (identifier) (attrpath (identifier))) (attrset (bind (attrpath (identifier)) (string)) (bind (attrpath (identifier)) (string)))))) (bind (attrpath (identifier)) (list (select (identifier) (attrpath (identifier))) (select (identifier) (attrpath (identifier))) (select (identifier) (attrpath (identifier))))) (bind (attrpath (identifier)) (app (select (identifier) (attrpath (identifier) (identifier))) (identifier))) (bind (attrpath (identifier)) (identifier)) (bind (attrpath (identifier)) (indented_string (interpolation (app (identifier) (identifier))))))))))
(Comment "# https://github.com/justinwoo/easy-purescript-nix/blob/7255d015b80d28c7c6db655dda215535cb2d4b41/purs.nix")
(SetFunction (Formals [(Formal (Identifier "pkgs") (Just (App (App (Identifier "import") (Spath "<nixpkgs>")) (AttrSet []))))]) (Let [(Bind (AttrPath "dynamic-linker") (Select (Identifier "pkgs") (AttrPath "stdenv.cc.bintools.dynamicLinker"))),(Bind (AttrPath "patchelf") (Function (Identifier "libPath") (If (Select (Identifier "pkgs") (AttrPath "stdenv.isDarwin")) (StringValue "\"\"") (StringIndented "''\n      chmod u+w $PURS\n      patchelf --interpreter ${dynamic-linker} --set-rpath ${libPath} $PURS\n      chmod u-w $PURS\n    ''"))))] [(App (Select (Identifier "pkgs") (AttrPath "stdenv.mkDerivation")) (RecAttrSet [(Bind (AttrPath "name") (StringValue "\"purs-simple\"")),(Bind (AttrPath "version") (StringValue "\"v0.12.3\"")),(Bind (AttrPath "src") (If (Select (Identifier "pkgs") (AttrPath "stdenv.isDarwin")) (App (Select (Identifier "pkgs") (AttrPath "fetchurl")) (AttrSet [(Bind (AttrPath "url") (StringValue "\"https://github.com/purescript/purescript/releases/download/v0.12.3/macos.tar.gz\"")),(Bind (AttrPath "sha256") (StringValue "\"1f916gv4fz571l4jvr15xjnsvjyy4nljv2ii9njwlm7k6yr5m0qn\""))])) (App (Select (Identifier "pkgs") (AttrPath "fetchurl")) (AttrSet [(Bind (AttrPath "url") (StringValue "\"https://github.com/purescript/purescript/releases/download/v0.12.3/linux64.tar.gz\"")),(Bind (AttrPath "sha256") (StringValue "\"1fad862a2sv4njxbbcfzibbi585m6is3ywb94nmjl8ax254baj3i\""))])))),(Bind (AttrPath "buildInputs") (List [(Select (Identifier "pkgs") (AttrPath "zlib")),(Select (Identifier "pkgs") (AttrPath "gmp")),(Select (Identifier "pkgs") (AttrPath "ncurses5"))])),(Bind (AttrPath "libPath") (App (Select (Identifier "pkgs") (AttrPath "lib.makeLibraryPath")) (Identifier "buildInputs"))),(Bind (AttrPath "dontStrip") (Identifier "true")),(Bind (AttrPath "installPhase") (StringIndented "''\n    mkdir -p $out/bin\n    PURS=\"$out/bin/purs\"\n\n    install -D -m555 -T purs $PURS\n    ${patchelf libPath}\n\n    mkdir -p $out/etc/bash_completion.d/\n    $PURS --bash-completion-script $PURS > $out/etc/bash_completion.d/purs-completion.bash\n  ''"))]))]))
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
(App (App (Identifier "import") (Path "./build.nix")) (AttrSet []))
import ./build.nix {}


(expression (let (bind (attrpath (identifier)) (integer)) (bind (attrpath (identifier)) (integer)) (binary (unary (identifier)) (identifier))))
(Let [(Bind (AttrPath "a") (Integer "123")),(Bind (AttrPath "b") (Integer "123"))] [(Binary (Unary "-" (Identifier "a")) "+" (Identifier "b"))])
let
  a = 123;

  b = 123;

in -a + b


(expression (function (formals (formal (identifier) (string)) (formal (identifier) (string))) (binary (identifier) (identifier))))
(SetFunction (Formals [(Formal (Identifier "foo") (Just (StringValue "\"foo\""))),(Formal (Identifier "bar") (Just (StringValue "\"bar\"")))]) (Binary (Identifier "foo") "+" (Identifier "bar")))
{ foo ? "foo", bar ? "bar" }:

foo + bar


(expression (function (formals (formal (identifier)) (formal (identifier)) (formal (identifier)) (formal (identifier)) (formal (identifier)) (formal (identifier)) (formal (identifier)) (formal (identifier)) (formal (identifier)) (formal (identifier)) (formal (identifier)) (formal (identifier)) (formal (identifier)) (formal (identifier)) (formal (identifier)) (formal (identifier)) (formal (identifier)) (formal (identifier)) (formal (identifier)) (formal (identifier)) (formal (identifier)) (formal (identifier))) (integer)))
(SetFunction (Formals [(Formal (Identifier "apple") Nothing),(Formal (Identifier "bpple") Nothing),(Formal (Identifier "cpple") Nothing),(Formal (Identifier "dpple") Nothing),(Formal (Identifier "epple") Nothing),(Formal (Identifier "fpple") Nothing),(Formal (Identifier "gpple") Nothing),(Formal (Identifier "hpple") Nothing),(Formal (Identifier "ipple") Nothing),(Formal (Identifier "jpple") Nothing),(Formal (Identifier "kpple") Nothing),(Formal (Identifier "lpple") Nothing),(Formal (Identifier "mpple") Nothing),(Formal (Identifier "npple") Nothing),(Formal (Identifier "opple") Nothing),(Formal (Identifier "ppple") Nothing),(Formal (Identifier "qpple") Nothing),(Formal (Identifier "rpple") Nothing),(Formal (Identifier "spple") Nothing),(Formal (Identifier "tpple") Nothing),(Formal (Identifier "upple") Nothing),(Formal (Identifier "vpple") Nothing)]) (Integer "1"))
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
(AttrSet [(Inherit [(Parens (Identifier "x")),(Attrs [(Identifier "a"),(Identifier "b"),(Identifier "c"),(Identifier "d")])]),(Bind (AttrPath "z") (With (Parens (Identifier "a")) (Integer "1"))),(Inherit [(Parens (Identifier "x")),(Attrs [(Identifier "apple"),(Identifier "bpple"),(Identifier "cpple"),(Identifier "dpple"),(Identifier "epple"),(Identifier "fpple"),(Identifier "gpple"),(Identifier "hpple"),(Identifier "ipple"),(Identifier "jpple"),(Identifier "kpple"),(Identifier "lpple"),(Identifier "mpple"),(Identifier "npple"),(Identifier "opple"),(Identifier "ppple"),(Identifier "qpple"),(Identifier "rpple"),(Identifier "spple"),(Identifier "tpple"),(Identifier "upple"),(Identifier "vpple")])])])
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
(Comment "# https://github.com/justinwoo/easy-purescript-nix/blob/7255d015b80d28c7c6db655dda215535cb2d4b41/psc-package2nix.nix")
(SetFunction (Formals [(Formal (Identifier "pkgs") (Just (App (App (Identifier "import") (Spath "<nixpkgs>")) (AttrSet []))))]) (App (App (Identifier "import") (Parens (App (Select (Identifier "pkgs") (AttrPath "fetchFromGitHub")) (AttrSet [(Bind (AttrPath "owner") (StringValue "\"justinwoo\"")),(Bind (AttrPath "repo") (StringValue "\"psc-package2nix\"")),(Bind (AttrPath "rev") (StringValue "\"b4d6a834ac124440a503f0510b8a9de95532b16c\"")),(Bind (AttrPath "sha256") (StringValue "\"0g9fq4j472bcr1x5na6mzr3av95xhvdmnlns1ncvsl4kqa8ix2zr\""))])))) (AttrSet [(Inherit [(Attrs [(Identifier "pkgs")])])])))
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
(AttrSet [(Bind (AttrPath "a") (List [(StringValue "\"apple\""),(StringValue "\"bpple\""),(StringValue "\"cpple\""),(StringValue "\"dpple\""),(StringValue "\"epple\""),(StringValue "\"fpple\""),(StringValue "\"gpple\""),(StringValue "\"hpple\""),(StringValue "\"ipple\""),(StringValue "\"jpple\""),(StringValue "\"kpple\""),(StringValue "\"lpple\""),(StringValue "\"mpple\""),(StringValue "\"npple\""),(StringValue "\"opple\""),(StringValue "\"ppple\""),(StringValue "\"qpple\""),(StringValue "\"rpple\""),(StringValue "\"spple\""),(StringValue "\"tpple\""),(StringValue "\"upple\""),(StringValue "\"vpple\"")])),(Bind (AttrPath "b") (List [(Integer "1"),(Integer "2"),(Integer "3"),(Integer "4"),(Integer "5")]))])
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
(Let [(Bind (AttrPath "moz_overlay") (App (Identifier "import") (Parens (App (Select (Identifier "builtins") (AttrPath "fetchTarball")) (Uri "https://github.com/mozilla/nixpkgs-mozilla/archive/master.tar.gz"))))),(Bind (AttrPath "nixpkgs") (App (App (Identifier "import") (Spath "<nixpkgs>")) (AttrSet [(Bind (AttrPath "overlays") (List [(Identifier "moz_overlay")]))])))] [(With (Identifier "nixpkgs") (App (Select (Identifier "stdenv") (AttrPath "mkDerivation")) (AttrSet [(Bind (AttrPath "name") (StringValue "\"moz_overlay_shell\"")),(Bind (AttrPath "buildInputs") (List [(Select (Identifier "nixpkgs") (AttrPath "latest.rustChannels.nightly.rust"))]))])))])
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
(Let [(Bind (AttrPath "nixpkgs") (App (Identifier "import") (Spath "<nixpkgs>"))),(Comment "# some comment")] [(With (Identifier "nixpkgs") (App (Select (Identifier "stdenv") (AttrPath "mkDerivation")) (AttrSet [(Bind (AttrPath "name") (StringValue "\"something\"")),(Bind (AttrPath "buildInputs") (List []))])))])
let
  nixpkgs = import <nixpkgs>;

  # some comment

in with nixpkgs; stdenv.mkDerivation {
  name = "something";

  buildInputs = [ ];
}


(expression (function (formals (formal (identifier)) (formal (identifier)) (ellipses)) (attrset (bind (attrpath (identifier) (identifier)) (string)))))
(SetFunction (Formals [(Formal (Identifier "config") Nothing),(Formal (Identifier "pkgs") Nothing),Ellipses]) (AttrSet [(Bind (AttrPath "system.stateVersion") (StringValue "\"19.09\""))]))
{ config, pkgs, ... }:

{
  system.stateVersion = "19.09";
}


(expression (let (bind (attrpath (identifier)) (string)) (bind (attrpath (identifier)) (string)) (indented_string (interpolation (identifier)) (interpolation (identifier)))))
(Let [(Bind (AttrPath "hello") (StringValue "\"123\"")),(Bind (AttrPath "world") (StringValue "\"456\""))] [(StringIndented "''\n  first: ${hello} second: ${world}\n''")])
let
  hello = "123";

  world = "456";

in ''
  first: ${hello} second: ${world}
''
