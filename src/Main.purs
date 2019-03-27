module Main where

import Prelude

import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Class.Console (log)
import FormatNix (TreeSitterParser, children, mkParser, nixLanguage, parse, printExpr, readNode, rootNode)

parser :: TreeSitterParser
parser = mkParser nixLanguage

input :: String
input = """
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
"""

main :: Effect Unit
main = do
  let node = rootNode $ parse parser input
  let nodes = readNode <$> children node

  traverse_ (log <<< printExpr) nodes

-- # https://github.com/justinwoo/easy-purescript-nix/blob/7255d015b80d28c7c6db655dda215535cb2d4b41/psc-package2nix.nix
-- { pkgs ? import <nixpkgs> {} } :

-- import ( pkgs.fetchFromGitHub {
--   owner = "justinwoo";
--   repo = "psc-package2nix";
--   rev = "b4d6a834ac124440a503f0510b8a9de95532b16c";
--   sha256 = "0g9fq4j472bcr1x5na6mzr3av95xhvdmnlns1ncvsl4kqa8ix2zr";
-- } ) {
--   inherit pkgs;
-- }
