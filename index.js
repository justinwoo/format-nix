const Parser = require("tree-sitter");
const Nix = require("tree-sitter-nix");

const parser = new Parser();
parser.setLanguage(Nix);

const sourceCode = `
# https://github.com/justinwoo/easy-purescript-nix/blob/7255d015b80d28c7c6db655dda215535cb2d4b41/purs.nix
{ pkgs ? import <nixpkgs> {} }:

let
  dynamic-linker = pkgs.stdenv.cc.bintools.dynamicLinker;

  patchelf = libPath :
    if pkgs.stdenv.isDarwin
      then ""
      else
        ''
          chmod u+w $PURS
          patchelf --interpreter \${dynamic-linker} --set-rpath \${libPath} $PURS
          chmod u-w $PURS
        '';

in pkgs.stdenv.mkDerivation rec {
  name = "purs-simple";
  version = "v0.12.3";

  src =
    if pkgs.stdenv.isDarwin
    then
    pkgs.fetchurl {
      url = "https://github.com/purescript/purescript/releases/download/v0.12.3/macos.tar.gz";
      sha256 = "1f916gv4fz571l4jvr15xjnsvjyy4nljv2ii9njwlm7k6yr5m0qn";
    }
    else
    pkgs.fetchurl {
      url = "https://github.com/purescript/purescript/releases/download/v0.12.3/linux64.tar.gz";
      sha256 = "1fad862a2sv4njxbbcfzibbi585m6is3ywb94nmjl8ax254baj3i";
    };


  buildInputs = [ pkgs.zlib
                  pkgs.gmp
                  pkgs.ncurses5];
  libPath = pkgs.lib.makeLibraryPath buildInputs;
  dontStrip = true;

  installPhase = ''
    mkdir -p $out/bin
    PURS="$out/bin/purs"

    install -D -m555 -T purs $PURS
    \${patchelf libPath}

    mkdir -p $out/etc/bash_completion.d/
    $PURS --bash-completion-script $PURS > $out/etc/bash_completion.d/purs-completion.bash
  '';
}
`;

const tree = parser.parse(sourceCode);

console.log('what is wtf')
console.log(tree.rootNode.toString());

module.exports = tree;

const visited = {};
function getTypes(indent, node) {
  if (node.type) {
    visited[node.type] = true;
  }
  if (node.text) {
    indentLog(indent, `type ${node.type} ${node.children.length} ${node.childCount}`);
    indentLog(indent, node.text.slice(0, 30).replace(/\n/g, '\\n'));
    console.log();
  }
  if (node.children) {
    node.children.map(x => getTypes(indent + 1, x));
  }
}

function indent(n, s) {
  if (n == 0) {
    return s;
  } else {
    return "  " + indent(n - 1, s);
  }
}

function indentLog(n, s) {
  return console.log(indent(n, s));
}

getTypes(0, tree.rootNode);
// console.log(Object.keys(visited));

// results
// [ 'expression',
//   'comment',
//   'function',
//   '{',
//   'formals',
//   'formal',
//   'identifier',
//   '?',
//   'app',
//   'spath',
//   'attrset',
//   '}',
//   ':',
//   '(',
//   'select',
//   '.',
//   'attrpath',
//   'bind',
//   '=',
//   'string',
//   '"',
//   ';',
//   ')',
//   'inherit',
//   'attrs' ]
