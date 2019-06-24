module Test.Main where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.String as String
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff, error, launchAff_, throwError)
import Effect.Aff as Aff
import Effect.Class.Console (log)
import FormatNix (TreeSitterParser, UnknownExpr(..), children, mkParser, nixLanguage, nodeToString, parse, printExpr, readNode, rootNode)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, writeTextFile)
import Node.Path (FilePath)

parser :: TreeSitterParser
parser = mkParser nixLanguage

processInput :: FilePath -> Aff String
processInput filepath = do
  input <- readTextFile UTF8 filepath
  let node = rootNode $ parse parser input
  let string = nodeToString node
  let eNodes = readNode `traverse` children node
  nodes <- case eNodes of
    Right xs -> pure xs
    Left (Unknown tag str) -> do
      throwError $ Aff.error $  "Error: contained unknown: " <> tag <> "\n" <> str
  let shown = Array.intercalate "\n" $ show <$> nodes
  let output = Array.intercalate "\n" $ printExpr <$> nodes

  log $ "printing " <> filepath <> ":"
  log string
  log shown
  log output
  log ""

  pure $ string <> "\n" <> shown <> "\n" <> output

main :: Effect Unit
main = launchAff_ do
  results <- traverse processInput
    [ "test/build.nix"
    , "test/import.nix"
    , "test/signs.nix"
    , "test/formals.nix"
    , "test/formals2.nix"
    , "test/inherits.nix"
    , "test/fetch-github.nix"
    , "test/list.nix"
    , "test/nixpkgs-mozilla.nix"
    , "test/let-with-comment.nix"
    , "test/formals-ellipses.nix"
    , "test/interpolation.nix"
    ]
  let output = Array.intercalate "\n\n" results
  writeTextFile UTF8 "test/output.nix" output
  if String.contains (String.Pattern "Unknown") output
    then throwError $ error "contained Unknowns"
    else pure unit
