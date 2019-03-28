module Test.Main where

import Prelude

import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class.Console (log)
import FormatNix (TreeSitterParser, children, mkParser, nixLanguage, parse, printExpr, readNode, rootNode)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Node.Path (FilePath)

parser :: TreeSitterParser
parser = mkParser nixLanguage

processInput :: FilePath -> Aff Unit
processInput filepath = do
  input <- readTextFile UTF8 filepath
  let node = rootNode $ parse parser input
  let nodes = readNode <$> children node
  log $ "printing " <> filepath <> ":"
  traverse_ (log <<< printExpr) nodes
  log ""

main :: Effect Unit
main = launchAff_ do
  processInput "test/build.nix"
  processInput "test/fetch-github.nix"
