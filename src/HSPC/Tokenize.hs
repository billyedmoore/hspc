module HSPC.Tokenize (tokenize, HSPCToken (..)) where

import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace, toUpper)
import Data.Int (Int64)
import Data.List (isPrefixOf)

data HSPCToken
  = LiteralIntTok Int64
  | LiteralBoolTok Bool
  | ProgramKeyWordTok
  | IntegerTypeTok
  | BooleanTypeTok
  | VarKeyWordTok
  | BeginKeyWordTok
  | EndKeyWordTok
  | HaltBuiltInTok
  | OpenBracketTok
  | CloseBracketTok
  | AssignmentTok
  | ColonTok
  | SemiColonTok
  | IntDivideTok
  | PlusTok
  | MinusTok
  | MultiplyTok
  | DivideTok
  | IdentifierTok String
  deriving (Show, Eq)

tokenize :: String -> [HSPCToken]
tokenize [] = []
tokenize ('{' : cs) = tokenize $ dropWhile (/= '}') cs
tokenize (c : cs)
  | "//" `isPrefixOf` (c : cs) = tokenize $ dropWhile (/= '\n') cs
  -- End includes this annoying '.'
  | "END." `isPrefixOf` map toUpper (c : cs) = EndKeyWordTok : tokenize (drop 3 cs)
  | isSpace c = tokenize cs
  | isDigit c =
      let (num, rest) = span isDigit (c : cs)
       in LiteralIntTok (read num) : tokenize rest
  | isAlpha c =
      let (word, rest) = span isAlphaNum (c : cs)
       in tokenizeIdentifierOrKeyWord (map toUpper word) : tokenize rest
  | otherwise = tokenizeOperator (c : cs)

tokenizeIdentifierOrKeyWord :: String -> HSPCToken
tokenizeIdentifierOrKeyWord "PROGRAM" = ProgramKeyWordTok
tokenizeIdentifierOrKeyWord "VAR" = VarKeyWordTok
tokenizeIdentifierOrKeyWord "BEGIN" = BeginKeyWordTok
tokenizeIdentifierOrKeyWord "HALT" = HaltBuiltInTok
tokenizeIdentifierOrKeyWord "DIV" = IntDivideTok
tokenizeIdentifierOrKeyWord "INTEGER" = IntegerTypeTok
tokenizeIdentifierOrKeyWord "BOOLEAN" = BooleanTypeTok
tokenizeIdentifierOrKeyWord "TRUE" = LiteralBoolTok True
tokenizeIdentifierOrKeyWord "FALSE" = LiteralBoolTok False
tokenizeIdentifierOrKeyWord s = IdentifierTok s

tokenizeOperator :: String -> [HSPCToken]
tokenizeOperator (':' : '=' : xs) = AssignmentTok : tokenize xs
tokenizeOperator (':' : xs) = ColonTok : tokenize xs
tokenizeOperator (';' : xs) = SemiColonTok : tokenize xs
tokenizeOperator ('(' : xs) = OpenBracketTok : tokenize xs
tokenizeOperator (')' : xs) = CloseBracketTok : tokenize xs
tokenizeOperator ('+' : xs) = PlusTok : tokenize xs
tokenizeOperator ('-' : xs) = MinusTok : tokenize xs
tokenizeOperator ('*' : xs) = MultiplyTok : tokenize xs
tokenizeOperator ('/' : xs) = DivideTok : tokenize xs
-- Ignore other symbols
tokenizeOperator (_ : xs) = tokenize xs
tokenizeOperator [] = []
