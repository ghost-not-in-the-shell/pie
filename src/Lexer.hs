module Lexer where
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token as Token

lexer ∷ TokenParser ()
lexer = makeTokenParser $ LanguageDef
  { commentStart   = "{-"
  , commentEnd     = "-}"
  , commentLine    = "--"
  , nestedComments = True
  , identStart     = letter
  , identLetter    = alphaNum
  , reservedNames  = [ "Set", "λ", "cons", "car", "cdr", "let", "in" ]
  , caseSensitive  = True
  }

whiteSpace ∷ Parser ()
whiteSpace = Token.whiteSpace lexer

identifier ∷ Parser String
identifier = Token.identifier lexer

reserved ∷ String → Parser ()
reserved = Token.reserved lexer

symbol ∷ String → Parser String
symbol = Token.symbol lexer

natural ∷ Parser Integer
natural = Token.natural lexer

parens ∷ Parser a → Parser a
parens = Token.parens lexer
