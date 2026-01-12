module HSPC.Parse (parse, AST (..), ParseError) where

import Data.Int (Int64)
import HSPC.Tokenize (HSPCToken (..))

data AST
  = Program String [AST]
  | Block [AST]
  | Identifier String
  | IntLiteral Int64
  | Halt Int64
  deriving (Show)

data ParseError
  = MustStartWithProgramStatement
  | ExpectedEndStatement
  | BlockMustStartWithBegin
  | NotImplemented [HSPCToken]
  deriving (Show)

parse :: [HSPCToken] -> Either ParseError AST
parse (ProgramKeyWordTok : IdentifierTok name : SemiColonTok : xs) = do
  block <- parseBlock xs
  Right $ Program name [block]
parse _ = Left MustStartWithProgramStatement

-- Blocks in pascal start with Begin and end with End.
parseBlock :: [HSPCToken] -> Either ParseError AST
parseBlock (BeginKeyWordTok : xs) = do
  (res, _) <- parseBlockInternal [] xs
  return res
parseBlock _ = Left BlockMustStartWithBegin

-- Parse a single expression and acc
parseBlockInternal :: [AST] -> [HSPCToken] -> Either ParseError (AST, [HSPCToken])
parseBlockInternal acc (EndKeyWordTok : xs) = Right (Block (reverse acc), xs)
parseBlockInternal _ [] = Left ExpectedEndStatement
parseBlockInternal acc xs = do
  (expr, rest) <- parseExpr xs
  parseBlockInternal (expr : acc) rest

parseExpr :: [HSPCToken] -> Either ParseError (AST, [HSPCToken])
parseExpr
  (HaltBuiltInTok : OpenBracketTok : LiteralIntTok i : CloseBracketTok : SemiColonTok : xs) = Right (Halt i, xs)
parseExpr tok = Left $ NotImplemented tok
