module Parser where
import Prelude hiding (pi, fst, snd)
import Data.Functor
import Text.Parsec hiding (label)
import Lexer
import Var
import Raw hiding (lam)

-- TERM ∷= CELL+ "→" TERM
--       | CELL+ "×" TERM
--       | "Case" ATOM ATOM | "Tag" ATOM
--       | "fst" ATOM | "snd" ATOM
--       | "cons" ATOM ATOM | "elimEnum" ATOM ATOM ATOM ATOM
--       | "su"   ATOM      | "elimTag" ATOM ATOM ATOM ATOM
--       | "arg" ATOM ATOM | "rec" ATOM
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
--       | "Set" | "Unit" | "Label" | "Enum" | "Desc"
--       | "nil"
--       | "ze"
--       | "end"
--       | "(" TERM "," TERM ")"
--       | "[" TERM* { "," TERM } "]"
--       | "{" TERM* { "," TERM } "}"
--       | "switch" TERM "{" BRANCH* { ";" BRANCH } "}"
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
tm₃ = case_ <|> tag <|> fst <|> snd <|> cons <|> elimEnum <|> su <|> suc <|> elimTag <|> arg <|> rec_ <|> app

cell ∷ Parser (Name , Raw)
cell = parens do
  x ← name ; colon
  𝕒 ← ty   ;
  return (x , 𝕒)

varOrCell ∷ Parser (Name , Maybe Raw)
varOrCell
  = (do
      x ← name
      return (x , Nothing)) <|>
    (do
      (x , 𝕒) ← cell
      return (x , Just 𝕒))

lam ∷ Parser Raw
lam = do
  void $ reserved "λ" <|> reserved "fun"
  xs   ← many1 varOrCell
  void $ symbol   "⇒" <|> symbol   "=>"
  body ← tm₀
  return $ foldr (uncurry Lam) body xs

let_ ∷ Parser Raw
let_ = do
  reserved "let"
  x    ← name ; colon
  𝕒    ← ty   ; symbol   "="
  t    ← tm₀  ; reserved "in"
  body ← tm₀
  return $ Let x 𝕒 t body

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

case_ ∷ Parser Raw
case_ = reserved "Case" >> Case <$> atom <*> atom

tag ∷ Parser Raw
tag = reserved "Tag" >> Tag <$> atom

fst ∷ Parser Raw
fst = reserved "fst" >> Fst <$> atom

snd ∷ Parser Raw
snd = reserved "snd" >> Snd <$> atom

cons ∷ Parser Raw
cons = reserved "cons" >> Cons <$> atom <*> atom

elimEnum ∷ Parser Raw
elimEnum = reserved "elimEnum" >> ElimEnum <$> atom <*> atom <*> atom <*> atom

su ∷ Parser Raw
su = reserved "su" >> Su <$> atom

suc ∷ Parser Raw
suc = reserved "suc" >> Suc <$> atom

elimTag ∷ Parser Raw
elimTag = reserved "elimTag" >> ElimTag <$> atom <*> atom <*> atom <*> atom

arg ∷ Parser Raw
arg = reserved "arg" >> Arg <$> atom <*> atom

rec_ ∷ Parser Raw
rec_ = reserved "rec" >> Rec <$> atom

app ∷ Parser Raw
app = do
  ts ← many1 atom
  return $ foldl1 App ts

atom ∷ Parser Raw
atom = var
   <|> set <|> unit <|> labelTy <|> enum <|> desc <|> Parser.nat
   <|> tick
   <|> sharp
   <|> nil
   <|> ze
   <|> zero
   <|> end
   <|> litNat
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

desc ∷ Parser Raw
desc = reserved "Desc" $> Desc

nat ∷ Parser Raw
nat = (reserved "ℕ" <|> reserved "Nat") $> Nat

tick ∷ Parser Raw
tick = Tick <$> label

sharp ∷ Parser Raw
sharp = Sharp <$> index

nil ∷ Parser Raw
nil = reserved "nil" $> Nil

ze ∷ Parser Raw
ze = reserved "ze" $> Ze

zero ∷ Parser Raw
zero = reserved "zero" $> Zero

end ∷ Parser Raw
end = reserved "end" $> End

litNat ∷ Parser Raw
litNat = Raw.nat <$> natural

pair ∷ Parser Raw
pair = try $ parens do
  t ← tm₀ ; void $ comma
  u ← tm₀
  return $ Pair t u

bracket ∷ Parser Raw
bracket = Bracket <$> brackets (commaSep tm₀)

brace ∷ Parser Raw
brace = Brace <$> braces (commaSep tm₀)

branch ∷ Parser (String , Raw)
branch = do
  l ← index
  void $ symbol "⇒" <|> symbol "=>"
  t ← tm₀
  return (l , t)

switch ∷ Parser Raw
switch = do
  reserved "switch"
  mot ← atom
  ts  ← braces (semiSep1 branch)
  let e = Brace $ map (Tick . (\ (a , _) → a)) ts
  return $ Lam "@t" (Just (Tag e)) $ Switch (Var "@t") mot ts
