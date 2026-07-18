module Parser where
import Prelude hiding (pi, fst, snd)
import Data.Functor
import Text.Parsec hiding (label)
import Lexer
import Var
import Raw

-- TERM ∷= CELL+ "→" TERM
--       | CELL+ "×" TERM
--       | "fst" ATOM | "snd" ATOM
--       | "Tag" ATOM | "su"  ATOM
--       | "Case" ATOM ATOM
--       | "λ"   NAME+ "⇒" TERM
--       | "let" NAME  ":" TERM "=" TERM "in" TERM
--       | APP
--
-- CELL ∷= "(" NAME ":" TERM ")"
--
-- APP  ∷= ATOM+
--
-- ATOM ∷= NAME
--       | LABEL
--       | INDEX
--       | "Set" | "Unit" | "Label" | "Enum"
--       | "ze"
--       | "(" TERM "," TERM ")"
--       | "[" TERM*   { "," TERM   } "]"
--       | "{" TERM*   { "," TERM   } "}"
--       | "{" BRANCH* { ";" BRANCH } "}"
--       | "(" TERM ")"
--
-- LABEL ∷= "'" ID
--
-- INDEX ∷= "#" ID
--
-- BRANCH ∷= INDEX "⇒" TERM

prog ∷ Parser Raw
prog = whiteSpace *> tm₀ <* eof

parseProg ∷ String → Either ParseError Raw
parseProg = parse prog "<stdin>"

ty ∷ Parser RTy
ty = tm₀

tm₀ = lam <|> let_ <|> tm₁
tm₁ = pi  <|> arrow
tm₂ = sg  <|> prod
tm₃ = fst <|> snd  <|> tag <|> su <|> case_ <|> app

lam ∷ Parser Raw
lam = do
  void $ reserved "λ" <|> reserved "fun"
  xs   ← many1 name
  void $ symbol   "⇒" <|> symbol   "=>"
  body ← tm₀
  return $ foldr Lam body xs

let_ ∷ Parser Raw
let_ = do
  reserved "let"
  x    ← name ; colon
  𝕒    ← ty   ; symbol   "="
  t    ← tm₀  ; reserved "in"
  body ← tm₀
  return $ Let x 𝕒 t body

cell ∷ Parser (Name , Raw)
cell = parens do
  x ← name ; colon
  𝕒 ← ty   ;
  return (x , 𝕒)

pi ∷ Parser RTy
pi = try do
  𝕒s ← many1 cell
  void $ symbol "→" <|> symbol "->"
  𝕓  ← tm₁
  return $ foldr (uncurry Pi) 𝕓 𝕒s

arrow ∷ Parser RTy
arrow = do
  𝕒 ← tm₂
  option 𝕒 do
    void $ symbol "→" <|> symbol "->"
    𝕓 ← tm₁
    return $ Pi "_" 𝕒 𝕓

sg ∷ Parser RTy
sg = try do
  𝕒s ← many1 cell
  void $ symbol "×" <|> symbol "**"
  𝕓  ← tm₂
  return $ foldr (uncurry Sg) 𝕓 𝕒s

prod ∷ Parser RTy
prod = do
  𝕒 ← tm₃
  option 𝕒 do
    void $ symbol "×" <|> symbol "**"
    𝕓 ← tm₂
    return $ Sg "_" 𝕒 𝕓

fst ∷ Parser Raw
fst = reserved "fst" >> Fst <$> atom

snd ∷ Parser Raw
snd = reserved "snd" >> Snd <$> atom

tag ∷ Parser Raw
tag = reserved "Tag" >> Tag <$> atom

su ∷ Parser Raw
su = reserved "su" >> Su <$> atom

case_ ∷ Parser Raw
case_ = reserved "Case" >> Case <$> atom <*> atom

app ∷ Parser Raw
app = do
  ts ← many1 atom
  return $ foldl1 App ts

atom ∷ Parser Raw
atom = var
   <|> set <|> unit <|> labelTy <|> enum
   <|> tick
   <|> ze
   <|> pair
   <|> bracket
   <|> brace
   <|> switch
   <|> parens tm₀

var ∷ Parser Raw
var = Var <$> name

set ∷ Parser RTy
set = reserved "Set" $> Set

unit ∷ Parser RTy
unit = (reserved "𝟙" <|> reserved "Unit") $> Unit

labelTy ∷ Parser Raw
labelTy = reserved "Label" $> Label

enum ∷ Parser Raw
enum = reserved "Enum" $> Enum

tick ∷ Parser Raw
tick = Tick <$> label

sharp ∷ Parser Raw
sharp = Sharp <$> index

ze ∷ Parser Raw
ze = reserved "ze" $> Ze

pair ∷ Parser Raw
pair = try $ parens do
  t ← tm₀ ; void $ comma
  u ← tm₀
  return $ Pair t u

bracket ∷ Parser Raw
bracket = Bracket <$> brackets (commaSep tm₀)

brace ∷ Parser Raw
brace = try $ Brace <$> braces (commaSep tm₀)

branch ∷ Parser (String , Raw)
branch = do
  l ← index
  void $ symbol "⇒" <|> symbol "=>"
  t ← tm₀
  return (l , t)

switch ∷ Parser Raw
switch = Switch <$> braces (semiSep1 branch)
