module FormatNix where

import Prelude

import Data.Array as Array
import Data.Foldable (surroundMap)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Unsafe.Coerce (unsafeCoerce)

data Expr
  -- top level
  = Expression (Array Expr)
  -- i just want to print comments
  | Comment String
  -- <nixpkgs>
  | Spath String
  -- "hi"
  | StringValue String
  -- Identifier
  | Identifier String
  -- spath
  | SPath String
  -- {, i am leaf
  | BraceLeft
  -- }, i am leaf
  | BraceRight
  -- (, i am leaf
  | ParenLeft
  -- ), i am leaf
  | ParenRight
  -- Colon, i am leaf
  | Colon
  -- Semicolon, i am leaf
  | Semicolon
  -- Function top level
  | Function (Array Expr)
  -- the main thing, the application of crap
  | App (Array Expr)
  -- a set
  | AttrSet (Array Expr)
  -- bind, e.g. owner = 1;
  | Bind (Array Expr)
  -- attrpath, only contains Identifier? e.g. owner of owner = 1;
  | AttrPath String
  -- inherit, note that first child might need to be thrown away ("inherit" literal)
  | Inherit (Array Expr)
  -- attributes for inherit?
  | Attrs (Array Expr)
  -- as in `set.attr`, note the leaf Dot in the middle
  | Select (Array Expr)
  -- Dot, i am a leaf
  | Dot
  -- =, i am a leaf
  | EqualSign
  -- set fn args, e.g. inside of braces { pkgs ? import <nixpkgs> {} }:
  | Formals (Array Expr)
  -- set fn arg, with identifier, quetion, app args
  | Formal (Array Expr)
  -- question mark of a formal, where the right side is the default value
  | QuestionMark
  -- unknown node type, with the type string and text contents
  | Unknown String String
derive instance eqExpr :: Eq Expr

-- | Node from tree-sitter
foreign import data Node :: Type

children :: Node -> Array Node
children tn = tn'.children
  where tn' = unsafeCoerce tn :: { children :: Array Node }

text :: Node -> String
text tn = tn'.text
  where tn' = unsafeCoerce tn :: { text :: String }

newtype TypeString = TypeString String
derive instance newtypeTypeString :: Newtype TypeString _

type_ :: Node -> TypeString
type_ tn = tn'."type"
  where tn' = unsafeCoerce tn :: { "type" :: TypeString }

foreign import data TreeSitterLanguage :: Type
foreign import nixLanguage :: TreeSitterLanguage

foreign import mkParser :: TreeSitterLanguage -> TreeSitterParser

foreign import data TreeSitterParser :: Type

foreign import parse :: TreeSitterParser -> String -> Tree

foreign import data Tree :: Type

rootNode :: Tree -> Node
rootNode tree = tree'.rootNode
  where tree' = unsafeCoerce tree :: { rootNode :: Node }

readNode :: Node -> Expr
readNode n = readNode' (type_ n) n

-- this is probably a tree-sitter-nix bug
notInheritNode :: Expr -> Boolean
notInheritNode (Inherit _) = false
notInheritNode _ = true

readChildren :: (Array Expr -> Expr) -> Node -> Expr
readChildren = \ctr n -> ctr $ readNode <$> children n

readNode' :: TypeString -> Node -> Expr
readNode' (TypeString "comment") n = Comment (text n)
readNode' (TypeString "function") n = readChildren Function n
readNode' (TypeString "formals") n = readChildren Formals n
readNode' (TypeString "formal") n = readChildren Formal n
readNode' (TypeString "attrset") n =  AttrSet $ removeBraces (readNode <$> children n)
readNode' (TypeString "attrs") n = readChildren Attrs n
readNode' (TypeString "app") n = readChildren App n
readNode' (TypeString "bind") n = Bind $ Array.filter (not eq Semicolon) (readNode <$> children n) 
readNode' (TypeString "inherit") n = Inherit $ Array.filter (not eq Semicolon && notInheritNode) (readNode <$> children n) 
readNode' (TypeString "select") n = Select $ Array.filter (not eq Dot) (readNode <$> children n)
readNode' (TypeString "attrpath") n = AttrPath (text n)
readNode' (TypeString "identifier") n = Identifier (text n)
readNode' (TypeString "spath") n = Spath (text n)
readNode' (TypeString "string") n = Spath (text n)
readNode' (TypeString "{") n = BraceLeft
readNode' (TypeString "}") n = BraceRight
readNode' (TypeString "(") n = ParenLeft
readNode' (TypeString ")") n = ParenRight
readNode' (TypeString "?") n = QuestionMark
readNode' (TypeString ":") n = Colon
readNode' (TypeString ";") n = Semicolon
readNode' (TypeString ".") n = Dot
readNode' (TypeString "=") n = EqualSign
readNode' (TypeString unknown) n = Unknown unknown (text n)

printExpr :: Expr -> String
printExpr = printExpr' 0

type Indentation = Int

indent :: Indentation -> String -> String
indent 0 s = s
indent 1 s = "  " <> s
indent n s = "  " <> indent (n - 1) s

withSep :: String -> Indentation -> Array Expr -> String
withSep = \sep i s -> Array.intercalate sep $ printExpr' i <$> s 

withSurround :: String -> Indentation -> Array Expr -> String
withSurround = \sep i s -> surroundMap sep (printExpr' i) s

printExpr' :: Indentation -> Expr -> String
printExpr' _ BraceLeft = "{"
printExpr' _ BraceRight = "}" 
printExpr' _ ParenLeft = "("
printExpr' _ ParenRight = ")" 
printExpr' _ QuestionMark = "?"
printExpr' _ EqualSign = "="
printExpr' _ Colon = ":"
printExpr' _ Semicolon = ";"
printExpr' _ (Comment str) = str
printExpr' _ (Identifier str) = str
printExpr' _ (Spath str) = str
printExpr' _ (AttrPath str) = str
printExpr' _ (StringValue str) = str
printExpr' _ (Unknown tag str) = "Unknown " <> tag <> " " <> str
printExpr' i (Expression exprs) = withSep "\n" i exprs
printExpr' i (Attrs exprs) = withSep "\n" i exprs
printExpr' i (AttrSet exprs) = if Array.null exprs
  then "{}"
  else "{" <> withSurround "\n" (i + 1) (removeBraces exprs) <> "}"
printExpr' i (Function exprs)
  -- set fns are formatted differently
  | Just BraceLeft <- Array.head exprs
  , length <- Array.length exprs
  , first <- Array.take (length - 1) exprs
  , Just last <- Array.last exprs = withSep " " i first <> "\n\n" <> printExpr' i last
  | otherwise = withSep " " i exprs
printExpr' i (Bind exprs) = indent i $ withSep " " i exprs <> ";"
printExpr' i (Inherit exprs) = indent i $ "inherit " <> withSep " " i exprs <> ";"
printExpr' i (App exprs) = withSep " " i exprs
printExpr' i (Formals exprs) = withSep " " i exprs
printExpr' i (Formal exprs) = withSep " " i exprs
printExpr' i (Select exprs) = withSep "." i exprs
printExpr' _ s = "Not handled yet: " <> getCtorName s

removeBraces :: Array Expr -> Array Expr
removeBraces = Array.filter (\x -> x /= BraceLeft && x /= BraceRight)

getCtorName :: Expr -> String
getCtorName e = e'.constructor.name
  where e' = unsafeCoerce e :: { constructor :: { name :: String } }
