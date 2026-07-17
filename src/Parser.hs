module Parser where
import Prelude hiding (pi, fst, snd)
import Data.Functor
import Text.Parsec hiding (label)
import Lexer
import Var
import Raw

-- TERM ∷= CELL+ "→" TERM
--       | CELL+ "×" TERM
--       | "pair" ATOM ATOM | "fst" ATOM | "snd" ATOM
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
--       | "Set" | "Unit" | "Label" | "Enum"
--       | "[" TERM*   { "," TERM   } "]"
--       | "{" TERM*   { "," TERM   } "}"
--       | "{" BRANCH* { ";" BRANCH } "}"
--       | "(" TERM ")"
--
-- LABEL ∷= "'" ID
--
-- BRANCH ∷= LABEL "⇒" TERM

prog ∷ Parser Raw
prog = whiteSpace *> tm₀ <* eof

parseProg ∷ String → Either ParseError Raw
parseProg = parse prog "<stdin>"

ty ∷ Parser RTy
ty = tm₀

tm₀ = lam  <|> let_ <|> tm₁
tm₁ = pi   <|> arrow
tm₂ = sg   <|> prod
tm₃ = pair <|> fst <|> snd  <|> app

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

pair ∷ Parser Raw
pair = reserved "pair" >> Pair <$> atom <*> atom

fst ∷ Parser Raw
fst = reserved "fst" >> Fst <$> atom

snd ∷ Parser Raw
snd = reserved "snd" >> Snd <$> atom

app ∷ Parser Raw
app = do
  ts ← many1 atom
  return $ foldl1 App ts

atom ∷ Parser Raw
atom = var
   <|> set <|> unit <|> labelTy <|> enum
   <|> tick   
   <|> bracket
   <|> brace
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

bracket ∷ Parser Raw
bracket = Bracket <$> brackets (commaSep tm₀)

brace ∷ Parser Raw
brace = Brace <$> braces (commaSep tm₀)
