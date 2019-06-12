module Parse (parser) where

import Text.ParserCombinators.Parsec
import Core
import Helper.Combinators
import Prelude hiding (print)

parser :: String -> Either ParseError Term
parser = parse machine ""

term :: Parser Term
term = try machine
  <|> try table
  <|> try rule
  <|> try symbol
  <|> try ident

ident :: Parser Term
ident = do
  c <- letter
  cs <- many (alphaNum <|> chars)
  return $ Ident (c : cs) -- TODO: doesn't check reserved keywords
  where
    chars = oneOf "!$%&|*+-/:<=>?@^_~#."

symbol :: Parser Term
symbol = do
  s <- many1 alphaNum
  return $ case s of
    "none" -> Symbol Blank
    "any" -> Symbol NotBlank
    _ -> Symbol (String s)

exec :: Parser [Term]
exec = try emptyBracket
  <|> try (underscore [Exec (Move N)])
  <|> try (between (char '[') (char ']') (sepBy (right <|> left <|> none <|> print <|> erase) (char ',' <* spaces)))
  where
    right = ch 'R' (Exec (Move R))
    left = ch 'L' (Exec (Move L))
    none = ch 'N' (Exec (Move N))
    erase = ch 'E' (Exec (Effect Erase))
    print = char 'P' >> (\s -> Exec (Effect (Print s))) <$> symbol -- TODO: how to avoid lambda?
    emptyBracket = string "[]" >> return [Exec (Move N)]

rule :: Parser Term
rule = do
  char '(' <* spaces
  mc <- ident <* spaces
  s <- (symbol <|> underscore (Symbol NotBlank)) <* spaces
  e <- exec <* spaces
  fc <- ident <* spaces
  char ')' <* spaces
  return $ Rule mc s e fc

table :: Parser Term
table = do
  char '(' <* spaces
  string "table" <* spaces
  rs <- sepBy term spaces
  char ')' <* spaces
  return $ Table (mkSeq rs)

machine :: Parser Term
machine = do
  char '(' <* spaces
  string "machine" <* spaces
  i <- ident <* spaces
  t <- term <* spaces
  char ')' <* spaces
  return $ Machine i t

ch :: Char -> a -> Parser a
ch c t = char c >> return t

underscore :: a -> Parser a
underscore = ch '_'