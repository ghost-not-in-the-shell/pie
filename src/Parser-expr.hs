module Parser where
import Prelude hiding (pi)
import Data.Functor
import Text.Parsec hiding (label)
import Text.Parsec.Expr
import Lexer
import Var
import Raw

prog ∷ Parser Raw
prog = whiteSpace *> tm <* eof

ty ∷ Parser RTy
ty = tm

tm ∷ Parser Raw
tm = (buildExpressionParser
  [ [ Prefix sg  , Infix  prod  AssocRight ]
  , [ Prefix pi  , Infix  arrow AssocRight ]
  , [ Prefix lam , Prefix let_             ]
  ] tm) <|> app

cell ∷ Parser (Name , Raw)
cell = parens do
  x ← name ; colon
  𝕒 ← ty   ;
  return (x , 𝕒)

sg ∷ Parser (RTy → RTy)
sg = try do
  𝕒s ← many1 cell
  void $ symbol "×" <|> symbol "**"
  return \ 𝕓 → foldr (uncurry Sg) 𝕓 𝕒s

prod ∷ Parser (RTy → RTy → RTy)
prod = try do
  void $ symbol "×" <|> symbol "**"
  return \ 𝕒 𝕓 → Sg "_" 𝕒 𝕓

pi ∷ Parser (RTy → RTy)
pi = try do
  𝕒s ← many1 cell
  void $ symbol "→" <|> symbol "->"
  return \ 𝕓 → foldr (uncurry Pi) 𝕓 𝕒s

arrow ∷ Parser (RTy → RTy → RTy)
arrow = try do
  void $ symbol "→" <|> symbol "->"
  return \ 𝕒 𝕓 → Pi "_" 𝕒 𝕓

lam ∷ Parser (Raw → Raw)
lam = do
  void $ reserved "λ" <|> reserved "fun"
  xs   ← many1 name
  void $ symbol   "⇒" <|> symbol   "=>"
  return \ body → foldr Lam body xs

let_ ∷ Parser (Raw → Raw)
let_ = do
  reserved "let"
  x  ← name ; colon
  𝕒  ← ty   ; symbol   "="
  t  ← tm   ; reserved "in"
  return \ body → Let x 𝕒 t body

app ∷ Parser Raw
app = do
  ts ← many1 atom
  return $ foldl1 App ts

atom ∷ Parser Raw
atom = var
   <|> tick
   <|> set <|> unit <|> labelTy <|> enum
   <|> bracket
   <|> brace
   <|> parens tm

var ∷ Parser Raw
var = Var <$> name

tick ∷ Parser Raw
tick = Tick <$> label

set ∷ Parser RTy
set = reserved "Set" $> Set

unit ∷ Parser RTy
unit = (reserved "𝟙" <|> reserved "Unit") $> Unit

labelTy ∷ Parser Raw
labelTy = reserved "Label" $> Label

enum ∷ Parser Raw
enum = reserved "Enum" $> Enum

bracket ∷ Parser Raw
bracket = Bracket <$> brackets (commaSep tm)

brace ∷ Parser Raw
brace = Brace <$> braces (commaSep tm)
