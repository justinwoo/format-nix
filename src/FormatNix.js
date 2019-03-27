const Parser = require("tree-sitter");
const Nix = require("tree-sitter-nix");

// foreign import nixLanguage :: TreeSitterLanguage
exports.nixLanguage = Nix;

// foreign import mkParser :: TreeSitterLanguage -> TreeSitterParser
exports.mkParser = function(language) {
  const parser = new Parser();
  parser.setLanguage(language);
  return parser;
};

// foreign import parse :: TreeSitterParser -> String -> Tree
exports.parse = function(parser) {
  return function(contents) {
    return parser.parse(contents);
  };
};
