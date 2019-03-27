const Parser = require("tree-sitter");
const Nix = require("tree-sitter-nix");

const parser = new Parser();
parser.setLanguage(Nix);

const sourceCode = `
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
`;

const tree = parser.parse(sourceCode);

console.log(tree.rootNode.toString());

module.exports = tree;

const visited = {};
function getTypes(indent, node) {
  if (node.type) {
    visited[node.type] = true;
  }
  if (node.text) {
    indentLog(indent, `type ${node.type} ${node.children.length}`);
    indentLog(indent, node.text);
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
