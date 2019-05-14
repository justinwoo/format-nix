module Main where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (error)
import FormatNix (TreeSitterParser, children, mkParser, nixLanguage, parse, printExpr, readNode, rootNode)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, writeTextFile)

foreign import argv :: Array String
foreign import _processExit :: Int -> Effect Unit

processExit :: Int -> Aff Unit
processExit = liftEffect <<< _processExit

parser :: TreeSitterParser
parser = mkParser nixLanguage

main :: Effect Unit
main = launchAff_ do
  case Array.index argv 2 of
    Nothing -> error needFileArg
    Just fileName -> do
      contents <- readTextFile UTF8 fileName
      let node = rootNode $ parse parser contents
      let nodes = readNode `traverse` children node
      case nodes of
        Right nodes' -> do
          let output = Array.intercalate "\n" $ printExpr <$> nodes'
          writeTextFile UTF8 fileName output
          error $ "formatted " <> fileName <> "."
          processExit 0
        Left _ -> do
          error "encountered errors in parsing input file."
          processExit 1
      pure unit

needFileArg :: String
needFileArg = """
You must provide an argument for an expression file to read and write to.

E.g. format-nix my-expression.nix
"""
