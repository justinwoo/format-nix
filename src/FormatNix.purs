module FormatNix where

import Prelude

import Data.Array as Array
import Data.Foldable (surroundMap)
import Data.List (List(..), (:))
import Data.List as List
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
  -- indented string
  | StringIndented String
  -- Identifier
  | Identifier String
  -- spath
  | SPath String
  -- {, i am leaf
  | BraceLeft
  -- }, i am leaf
  | BraceRight
  -- [, i am leaf
  | BracketLeft
  -- ], i am leaf
  | BracketRight
  -- (, i am leaf
  | ParenLeft
  -- ), i am leaf
  | ParenRight
  -- Colon, i am leaf
  | Colon
  -- Semicolon, i am leaf
  | Semicolon
  -- function, input_expr: output_expr
  | Function Expr Expr
  -- set function, { formals }: output_expr
  | SetFunction Expr Expr
  -- the main thing, the application of crap
  | App (Array Expr)
  -- let expr in expr
  | Let Expr Expr
  -- if cond_exprs then_exprs else_exprs
  | If Expr Expr Expr
  -- then, leaf
  | Then
  -- else, leaf
  | Else
  -- in of let-in, leaf
  | In
  -- a set
  | AttrSet (Array Expr)
  -- a recursive attr set
  | RecAttrSet (Array Expr)
  -- quantity
  | Quantity Expr
  -- a list
  | List (Array Expr)
  -- bind, e.g. owner = 1;
  | Bind Expr Expr
  -- multiple bind
  | Binds (Array Expr)
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
  -- set fn arg with an identifier, where it may or may not have a default value expr
  | Formal Expr (Maybe Expr)
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

-- | Filter for named children
namedChildren :: Node -> Array Node
namedChildren = Array.filter isNamed <<< children

-- | Is a given Node Real or is it fake?
isNamed :: Node -> Boolean
isNamed tn = tn'.isNamed
  where tn' = unsafeCoerce tn :: { isNamed :: Boolean }

text :: Node -> String
text tn = tn'.text
  where tn' = unsafeCoerce tn :: { text :: String }

newtype TypeString = TypeString String
derive instance newtypeTypeString :: Newtype TypeString _
derive newtype instance eqTypeString :: Eq TypeString

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

readChildren :: (Array Expr -> Expr) -> Node -> Expr
readChildren = \ctr n -> ctr $ readNode <$> namedChildren n

readNode' :: TypeString -> Node -> Expr
readNode' (TypeString "comment") n = Comment (text n)
readNode' (TypeString "function") n
  | children' <- namedChildren n
  , (input : output : Nil ) <- List.fromFoldable (readNode <$> namedChildren n)
    = case input of
        Formals _ -> SetFunction input output
        _ -> Function input output
  | otherwise = Unknown "function variation" (text n)
readNode' (TypeString "formals") n = readChildren Formals n
readNode' (TypeString "formal") n
  | children' <- List.fromFoldable (readNode <$> namedChildren n)
  = case children' of
      identifier : Nil -> Formal identifier Nothing
      identifier : default : Nil -> Formal identifier (Just default)
      _ -> Unknown "formal varigation" (text n)
readNode' (TypeString "binds") n = readChildren Binds n
readNode' (TypeString "attrset") n =  AttrSet $ removeBraces (readNode <$> namedChildren n)
readNode' (TypeString "list") n =  List $ removeBrackets (readNode <$> namedChildren n)
readNode' (TypeString "rec_attrset") n =  RecAttrSet $ removeBraces (readNode <$> namedChildren n)
readNode' (TypeString "attrs") n = readChildren Attrs n
readNode' (TypeString "app") n = readChildren App n
readNode' (TypeString "if") n
  | children' <- namedChildren n
  , (cond : then_ : else_ : Nil ) <- List.fromFoldable (namedChildren n)
    = If (readNode cond) (readNode then_) (readNode else_)
  | otherwise = Unknown "if variation" (text n)
readNode' (TypeString "let") n
  | children' <- namedChildren n
  , (binds : app : Nil ) <- List.fromFoldable (namedChildren n)
    = Let (readNode binds) (readNode app)
  | otherwise = Unknown "let variation" (text n)
readNode' (TypeString "quantity") n
  | children' <- namedChildren n
  , expr : Nil <- List.fromFoldable (readNode <$> namedChildren n)
    = Quantity expr
  | otherwise = Unknown "quantity variation" (text n)
readNode' (TypeString "bind") n
  | children' <- namedChildren n
  , name : value : Nil <- List.fromFoldable (readNode <$> namedChildren n)
    = Bind name value
  | otherwise = Unknown "Bind variation" (text n)
readNode' (TypeString "inherit") n = Inherit $ readNode <$> namedChildren n 
readNode' (TypeString "select") n = Select $ readNode <$> namedChildren n
readNode' (TypeString "attrpath") n = AttrPath (text n)
readNode' (TypeString "identifier") n = Identifier (text n)
readNode' (TypeString "spath") n = Spath (text n)
readNode' (TypeString "string") n = StringValue (text n)
readNode' (TypeString "indented_string") n = StringIndented (text n)
readNode' (TypeString "in") n = In
readNode' (TypeString "then") n = Then
readNode' (TypeString "else") n = Else
-- readNode' (TypeString "{") n = BraceLeft
-- readNode' (TypeString "}") n = BraceRight
-- readNode' (TypeString "[") n = BracketLeft
-- readNode' (TypeString "]") n = BracketRight
-- readNode' (TypeString "(") n = ParenLeft
-- readNode' (TypeString ")") n = ParenRight
-- readNode' (TypeString "?") n = QuestionMark
-- readNode' (TypeString ":") n = Colon
-- readNode' (TypeString ";") n = Semicolon
-- readNode' (TypeString ".") n = Dot
-- readNode' (TypeString "=") n = EqualSign
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
printExpr' _ In = "in"
printExpr' _ Then = "then" 
printExpr' _ Else = "else" 
printExpr' _ (Comment str) = str
printExpr' _ (Identifier str) = str
printExpr' _ (Spath str) = str
printExpr' _ (AttrPath str) = str
printExpr' _ (StringValue str) = str
printExpr' i (StringIndented str) = "\n" <> indent (i + 1) str
printExpr' _ (Unknown tag str) = "Unknown " <> tag <> " " <> str
printExpr' i (Expression exprs) = withSep "\n" i exprs
printExpr' i (List exprs) = left <> withSep sep i'' exprs <> right
  where
    i' = i + 1
    i'' = i + 2
    sep = "\n" <> indent i' ""
    left = "[" <> sep
    right = "\n" <> indent i "]"
printExpr' i (Attrs exprs) = withSep "\n" i exprs
printExpr' i (AttrSet exprs) = if Array.null exprs
  then "{}"
  else "{" <> withSurround "\n" (i + 1) (removeBraces exprs) <> indent i "}"
printExpr' i (RecAttrSet exprs) = "rec " <> printExpr' i (AttrSet exprs)
printExpr' i (SetFunction input output) = "{ " <> input_ <> " }:\n\n" <> output_
  where
    input_ = printExpr' i input
    output_ = printExpr' i output
printExpr' i (Function input output) = input_ <> ": " <> output_
  where
    input_ = printExpr' i input
    output_ = printExpr' i output
printExpr' i (Let binds expr) = let_ <> binds' <> in_ <> expr'
  where
    let_ = indent i "let\n"
    in_ = "\n" <> indent i "in\n" <> indent (i + 1) ""
    binds' = printExpr' (i + 1) binds
    expr' = printExpr' (i + 1) expr
printExpr' i (If cond first second) = if_ <> then_ <> else_
  where
    i' = i + 1
    i'' = i + 2
    if_ = "if " <> printExpr' i cond
    then_ = append "\n" $ indent i' "then " <> printExpr' i' first
    else_ = append "\n" $ indent i' "else " <> printExpr' i' second
printExpr' i (Quantity expr) = "(" <> printExpr' i expr <> ")"
printExpr' i (Binds exprs) = withSep "\n" i exprs
printExpr' i (Bind name value) = indent i $ printExpr' i name <> " = " <> printExpr' i value <> ";"
printExpr' i (Inherit exprs) = indent i $ "inherit " <> withSep " " i exprs <> ";"
printExpr' i (App exprs) = withSep " " i exprs
printExpr' i (Formals exprs) = withSep " " i exprs
printExpr' i (Formal identifier Nothing) = printExpr' i identifier
printExpr' i (Formal identifier (Just value)) = printExpr' i identifier <> " ? " <> printExpr' i value
printExpr' i (Select exprs) = withSep "." i exprs
printExpr' _ s = "Unknown/Not handled yet: " <> getCtorName s

removeBraces :: Array Expr -> Array Expr
removeBraces = Array.filter (\x -> x /= BraceLeft && x /= BraceRight)

removeBrackets :: Array Expr -> Array Expr
removeBrackets = Array.filter (\x -> x /= BracketLeft && x /= BracketRight)

getCtorName :: Expr -> String
getCtorName e = e'.constructor.name
  where e' = unsafeCoerce e :: { constructor :: { name :: String } }
