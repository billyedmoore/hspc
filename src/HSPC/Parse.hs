module HSPC.Parse (parse, Program (..), Statement (..), Expression (..), ParseError) where

import Data.Int (Int64)
import HSPC.Tokenize (HSPCToken (..))

data Program = Program String [Statement]
  deriving (Show, Eq)

data DataType = Integer Int64
  deriving (Show, Eq)

data Statement
  = VarDecl String DataType
  | VarAssign String Expression
  | Block [Statement]
  | Halt Expression
  | ExprStmt Expression
  deriving (Show, Eq)

data Expression
  = Identifier String
  | IntLiteral Int64
  | Add Expression Expression
  | Subtract Expression Expression
  | Multiply Expression Expression
  | IntDivide Expression Expression
  | UnaryPlus Expression
  | UnaryMinus Expression
  deriving (Show, Eq)

data ParseError
  = MustStartWithProgramStatement
  | ExpectedEndStatement
  | BlockMustStartWithBegin
  | BracketNotClosed
  | ExpectedOperand String
  | UnmatchedBracket
  | UnexpectedToken HSPCToken
  | ExpectedToken HSPCToken
  | NotImplemented [HSPCToken]
  deriving (Show)

parse :: [HSPCToken] -> Either ParseError Program
parse (ProgramKeyWordTok : IdentifierTok name : SemiColonTok : xs) = do
  block <- parseBlock xs
  Right $ Program name [block]
parse _ = Left MustStartWithProgramStatement

-- Blocks in pascal start with Begin and end with End.
parseBlock :: [HSPCToken] -> Either ParseError Statement
parseBlock (BeginKeyWordTok : xs) = do
  (res, _) <- parseBlockInternal [] xs
  return res
parseBlock _ = Left BlockMustStartWithBegin

-- Parse a single expression and acc
parseBlockInternal :: [Statement] -> [HSPCToken] -> Either ParseError (Statement, [HSPCToken])
parseBlockInternal acc (EndKeyWordTok : xs) = Right (Block (reverse acc), xs)
parseBlockInternal _ [] = Left ExpectedEndStatement
parseBlockInternal acc xs = do
  (expr, rest) <- parseStatement xs
  parseBlockInternal (expr : acc) rest

breakLastOuter :: (HSPCToken -> Bool) -> [HSPCToken] -> ([HSPCToken], [HSPCToken])
breakLastOuter breakCond toks = go (reverse toks) [] 0
  where
    go :: [HSPCToken] -> [HSPCToken] -> Int -> ([HSPCToken], [HSPCToken])
    go [] _ _ = (toks, [])
    go (CloseBracketTok : rest) acc i = go rest (CloseBracketTok : acc) (i + 1)
    go (OpenBracketTok : rest) acc i = go rest (OpenBracketTok : acc) (i - 1)
    go (lastTok : rest) acc 0
      | breakCond lastTok = (reverse rest, lastTok : acc)
    go (lastTok : rest) acc i = go rest (lastTok : acc) i

splitToMatchingBracket :: [HSPCToken] -> Either ParseError ([HSPCToken], [HSPCToken])
splitToMatchingBracket toks = go toks [] 0
  where
    go :: [HSPCToken] -> [HSPCToken] -> Int -> Either ParseError ([HSPCToken], [HSPCToken])
    go (CloseBracketTok : xs) acc 0 = Right (reverse acc, xs)
    go (CloseBracketTok : xs) acc i =
      go xs (CloseBracketTok : acc) (i - 1)
    go (OpenBracketTok : xs) acc i =
      go xs (OpenBracketTok : acc) (i + 1)
    go (x : xs) acc i = go xs (x : acc) i
    go [] _ _ = Left UnmatchedBracket

parseStatement :: [HSPCToken] -> Either ParseError (Statement, [HSPCToken])
parseStatement
  (HaltBuiltInTok : OpenBracketTok : CloseBracketTok : SemiColonTok : xs) = Right (Halt (IntLiteral 0), xs)
parseStatement
  (HaltBuiltInTok : OpenBracketTok : xs) = do
    (internal, rawRest) <- splitToMatchingBracket xs
    rest <- removeSemiColonHead rawRest
    op <- parseIntOperand internal
    return (Halt op, rest)
    where
      removeSemiColonHead :: [HSPCToken] -> Either ParseError [HSPCToken]
      removeSemiColonHead (SemiColonTok : rest) = Right rest
      removeSemiColonHead _ = Left $ ExpectedToken SemiColonTok
parseStatement tok = Left $ NotImplemented tok

-- IntOperand  = Term { ("+" | "-") Term }
-- Term        = Factor { ("*" | "div") Factor }
-- Factor      = ("+" | "-") Factor | Number | "(" IntOperand ")"
-- Number      = // an integer
parseIntOperand :: [HSPCToken] -> Either ParseError Expression
parseIntOperand toks = case breakLastOuter (`elem` [PlusTok, MinusTok]) toks of
  -- starts with +-
  ([], after) -> parseTerm after
  -- doesn't contain +-
  (before, []) -> parseTerm before
  (_, [PlusTok]) -> Left $ ExpectedOperand "Expected operand after +"
  (before, PlusTok : after) -> do
    left <- parseIntOperand before
    right <- parseTerm after
    return $ Add left right
  (_, [MinusTok]) -> Left $ ExpectedOperand "Expected operand after -"
  (before, MinusTok : after) -> do
    left <- parseIntOperand before
    right <- parseTerm after
    return $ Subtract left right
  _ -> error "Should be unreachable since we break on PlusTok || MinusTok"

parseTerm :: [HSPCToken] -> Either ParseError Expression
parseTerm toks = case breakLastOuter (`elem` [IntDivideTok, MultiplyTok]) toks of
  (before, []) -> parseFactor before
  (_, [IntDivideTok]) -> Left $ ExpectedOperand "Expected operand after 'div'"
  (before, IntDivideTok : after) -> do
    left <- parseIntOperand before
    right <- parseFactor after
    return $ IntDivide left right
  (_, [MultiplyTok]) -> Left $ ExpectedOperand "Expected operand after *"
  (before, MultiplyTok : after) -> do
    left <- parseIntOperand before
    right <- parseFactor after
    return $ Multiply left right
  _ -> error "Should be unreachable since we break on IntDivideTok || MultiplyTok"

parseFactor :: [HSPCToken] -> Either ParseError Expression
parseFactor [LiteralIntTok n] = Right $ IntLiteral n
parseFactor (PlusTok : xs) = do
  operand <- parseFactor xs
  return $ UnaryPlus operand
parseFactor (MinusTok : xs) = do
  operand <- parseFactor xs
  return $ UnaryMinus operand
parseFactor (OpenBracketTok : xs) = do
  (inside, remaining) <- splitToMatchingBracket xs
  case remaining of
    [] -> parseIntOperand inside
    (tok : _) -> Left $ UnexpectedToken tok
parseFactor (tok : _) = Left $ UnexpectedToken tok
parseFactor [] = Left $ ExpectedOperand "Expected operand"
