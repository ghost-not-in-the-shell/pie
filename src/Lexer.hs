module Lexer where
import Text.Parsec
import Text.Parsec.Token as Token
import Var

type Parser = Parsec String Int

lexer ∷ TokenParser Int
lexer = makeTokenParser LanguageDef
  { commentStart    = "{-"
  , commentEnd      = "-}"
  , commentLine     = "--"
  , nestedComments  = True
  , caseSensitive   = True
  , opStart         = undefined
  , opLetter        = undefined
  , reservedOpNames = undefined
  , identStart      = letter
  , identLetter     = alphaNum <|> char '_' <|> char '\''
  , reservedNames   = [ "Set", "𝟙", "Unit", "Label", "Enum", "Desc", "ℕ", "Tag", "Case", "Hyps"
                      , "λ", "fun", "fst", "snd", "nil", "cons", "elimEnum", "ze", "su", "zero", "suc", "elimTag", "switch", "end", "arg", "rec", "El", "elim", "μ", "Mu", "inj"
                      , "let", "in" ]
  }

name ∷ Parser Name
name = Token.identifier lexer

label ∷ Parser String
label = char '\'' >> Token.identifier lexer

index ∷ Parser String
index = char '#' >> Token.identifier lexer

reserved ∷ String → Parser ()
reserved = Token.reserved lexer

natural ∷ Parser Integer
natural = Token.natural lexer

symbol ∷ String → Parser String
symbol = Token.symbol lexer

whiteSpace ∷ Parser ()
whiteSpace = Token.whiteSpace lexer

parens ∷ Parser a → Parser a
parens = Token.parens lexer

braces ∷ Parser a → Parser a
braces = Token.braces lexer

brackets ∷ Parser a → Parser a
brackets = Token.brackets lexer

semi ∷ Parser String
semi = Token.semi lexer

comma ∷ Parser String
comma = Token.comma lexer

colon ∷ Parser String
colon = Token.colon lexer

dot ∷ Parser String
dot = Token.dot lexer

semiSep ∷ Parser a → Parser [a]
semiSep = Token.semiSep lexer

semiSep1 ∷ Parser a → Parser [a]
semiSep1 = Token.semiSep1 lexer

commaSep ∷ Parser a → Parser [a]
commaSep = Token.commaSep lexer

commaSep1 ∷ Parser a → Parser [a]
commaSep1 = Token.commaSep1 lexer
