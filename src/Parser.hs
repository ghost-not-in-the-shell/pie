module Parser where
import Text.ParserCombinators.Parsec
import Raw
import Lexer

atom ∷ Parser Raw
atom = set <|> var <|> hole <|> lam <|> parens expr

expr ∷ Parser Raw
expr = pi_ <|> app <|> sg <|> cons <|> car <|> cdr <|> let_

prog ∷ Parser Raw
prog = whiteSpace *> expr <* eof

cell ∷ Parser (String, Raw)
cell = parens $ do
  { x  ← identifier <|> symbol "_"
  ; symbol ":"
  ; _A ← expr
  ; return (x,_A)
  }

var ∷ Parser Raw
var = Var <$> identifier

hole ∷ Parser Raw
hole = Hole <$ symbol "_"

set ∷ Parser Raw
set = Set <$ reserved "Set"

pi_ ∷ Parser Raw
pi_ = do
  symbol "∀"
  _As ← many1 cell
  symbol "→"
  _B  ← expr
  return $ foldr (uncurry Pi) _B _As

lam ∷ Parser Raw
lam = do
  symbol "λ"
  xs ← many1 (identifier <|> symbol "_")
  symbol "→"
  t  ← expr
  return $ foldr Lam t xs

app ∷ Parser Raw
app = do
  ts ← many1 atom
  return $ foldl1 App ts

sg ∷ Parser Raw
sg = do
  symbol "∃"
  _As ← many1 cell
  symbol "×"
  _B  ← expr
  return $ foldr (uncurry Sg) _B _As

cons ∷ Parser Raw
cons = do
  reserved "cons"
  t ← atom
  u ← atom
  return $ Cons t u

car ∷ Parser Raw
car = do
  reserved "car"
  t ← atom
  return $ Car t

cdr ∷ Parser Raw
cdr = do
  reserved "cdr"
  t ← atom
  return $ Cdr t

let_ ∷ Parser Raw
let_ = do
  reserved "let"
  x  ← identifier
  symbol ":"
  _A ← expr
  symbol "="
  t  ← expr
  reserved "in"
  u  ← expr
  return $ Let x _A t u
