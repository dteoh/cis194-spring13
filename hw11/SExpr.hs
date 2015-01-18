{- CIS 194 HW 11
   due Monday, 8 April
-}

module SExpr where

import AParser
import Control.Applicative
import Data.Char (isAlpha, isAlphaNum, isSpace)

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore = many

oneOrMore :: Parser a -> Parser [a]
oneOrMore = some


------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore $ satisfy isSpace

ident :: Parser String
ident = (++) <$> (oneOrMore $ satisfy isAlpha) <*> (zeroOrMore $ satisfy isAlphaNum)

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show

parseSExpr :: Parser SExpr
parseSExpr = A <$> parseAtom <|> Comb <$> parseList
  where parseList = parseLParen *> (oneOrMore parseSExpr) <* parseRParen

parseAtom :: Parser Atom
parseAtom = parseInteger <|> parseIdent
  where parseInteger = N <$> (spaces *> posInt <* spaces)
        parseIdent   = I <$> (spaces *> ident <* spaces)

parseLParen = parseParen '('
parseRParen = parseParen ')'

parseParen :: Char -> Parser Char
parseParen p = spaces *> char p <* spaces

