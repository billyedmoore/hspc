module HSPC.Tokenize (tokenize, HSPCToken (..)) where

import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Data.Int (Int64)
import Data.List (isPrefixOf)

data HSPCToken
  = LiteralIntTok Int64
  | ProgramKeyWordTok
  | BeginKeyWordTok
  | EndKeyWordTok
  | HaltBuiltInTok
  | OpenBracketTok
  | CloseBracketTok
  | SemiColonTok
  | IdentifierTok String
  deriving (Show)

tokenize :: String -> [HSPCToken]
tokenize [] = []
tokenize (c : cs)
  | "//" `isPrefixOf` (c : cs) = tokenize $ dropWhile (/= '\n') cs
  | isSpace c = tokenize cs
  | isDigit c =
      let (num, rest) = span isDigit (c : cs)
       in LiteralIntTok (read num) : tokenize rest
  | isAlpha c =
      let (word, rest) = span isAlphaNum (c : cs)
       in tokenizeIdentifierOrKeyWord word : tokenize rest
  | otherwise = tokenizeOperator (c : cs)

tokenizeIdentifierOrKeyWord :: String -> HSPCToken
tokenizeIdentifierOrKeyWord "program" = ProgramKeyWordTok
tokenizeIdentifierOrKeyWord "begin" = BeginKeyWordTok
tokenizeIdentifierOrKeyWord "end." = EndKeyWordTok
tokenizeIdentifierOrKeyWord ident = IdentifierTok ident

tokenizeOperator :: String -> [HSPCToken]
tokenizeOperator (';' : xs) = SemiColonTok : tokenize xs
tokenizeOperator ('(' : xs) = OpenBracketTok : tokenize xs
tokenizeOperator (')' : xs) = CloseBracketTok : tokenize xs
-- Ignore other symbols
tokenizeOperator (_ : xs) = tokenize xs
tokenizeOperator [] = []
