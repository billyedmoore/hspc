module HSPC.Parse
  ( parse,
    Program (..),
    Block (..),
    Statement (..),
    Expression (..),
    ParseError,
    HSPCDataType (..),
  )
where

import Data.Int (Int64)
import HSPC.Tokenize (HSPCToken (..))

data Program = Program String [Block]
  deriving (Show, Eq)

data HSPCDataType = IntegerType
  deriving (Show, Eq)

data Block
  = -- Block locals body
    MainProgramBlock [(String, HSPCDataType)] [Statement]
  deriving (Show, Eq)

data Statement
  = VarAssign String Expression
  | Assignment String Expression
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
  | InvalidStartOfBlock
  | BracketNotClosed
  | ExpectedVariableDeclaration [HSPCToken]
  | ExpectedOperand String
  | UnmatchedBracket
  | InvalidDataType HSPCToken
  | UnexpectedToken HSPCToken
  | ExpectedToken HSPCToken
  | NotImplemented [HSPCToken]
  deriving (Show)

toDataType :: HSPCToken -> Either ParseError HSPCDataType
toDataType IntegerTypeTok = Right IntegerType
toDataType toc = Left $ InvalidDataType toc

parse :: [HSPCToken] -> Either ParseError Program
parse (ProgramKeyWordTok : IdentifierTok name : SemiColonTok : xs) = do
  block <- parseMainProgramBlock xs
  Right $ Program name [block]
parse _ = Left MustStartWithProgramStatement

-- Block = {VarSection} "BEGIN" BlockInternal
-- VarSection = "VAR" {VarDeclaration}
-- BlockInternal = {Statement} "END."
parseMainProgramBlock :: [HSPCToken] -> Either ParseError Block
parseMainProgramBlock (VarKeyWordTok : tocs) = do
  (vars, rest) <- parseVariableDeclarations [] tocs
  (res, _) <- parseStatements [] rest
  return $ MainProgramBlock vars res
parseMainProgramBlock (BeginKeyWordTok : tocs) = do
  (res, _) <- parseStatements [] tocs
  return $ MainProgramBlock [] res
parseMainProgramBlock _ = Left InvalidStartOfBlock

removeLeadingSemiColon :: [HSPCToken] -> Either ParseError [HSPCToken]
removeLeadingSemiColon (SemiColonTok : rest) = Right rest
removeLeadingSemiColon _ = Left $ ExpectedToken SemiColonTok

-- Standard pascal form i.e. cannot be initialized in the var block
parseVariableDeclarations :: [(String, HSPCDataType)] -> [HSPCToken] -> Either ParseError ([(String, HSPCDataType)], [HSPCToken])
parseVariableDeclarations acc (BeginKeyWordTok : xs) = Right (reverse acc, xs)
parseVariableDeclarations acc xs = do
  (expr, rest) <- parseVariableDeclaration xs
  parseVariableDeclarations (expr : acc) rest

parseVariableDeclaration :: [HSPCToken] -> Either ParseError ((String, HSPCDataType), [HSPCToken])
-- TODO: Handle multiple identifiers seperated by commas
parseVariableDeclaration (IdentifierTok name : ColonTok : dataTypeToc : xs) = do
  datatype <- toDataType dataTypeToc
  rest <- removeLeadingSemiColon xs
  return ((name, datatype), rest)
parseVariableDeclaration toks = Left $ ExpectedVariableDeclaration toks

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

parseStatements :: [Statement] -> [HSPCToken] -> Either ParseError ([Statement], [HSPCToken])
parseStatements acc (EndKeyWordTok : xs) = Right (reverse acc, xs)
parseStatements _ [] = Left ExpectedEndStatement
parseStatements acc xs = do
  (expr, rest) <- parseStatement xs
  parseStatements (expr : acc) rest

parseStatement :: [HSPCToken] -> Either ParseError (Statement, [HSPCToken])
parseStatement (IdentifierTok name : AssignmentTok : xs) = do
  let (rhs, rawRest) = break (== SemiColonTok) xs
  rest <- removeLeadingSemiColon rawRest
  op <- parseIntOperand rhs
  return (Assignment name op, rest)
parseStatement
  (HaltBuiltInTok : OpenBracketTok : CloseBracketTok : SemiColonTok : xs) = Right (Halt (IntLiteral 0), xs)
parseStatement
  (HaltBuiltInTok : OpenBracketTok : xs) = do
    (internal, rawRest) <- splitToMatchingBracket xs
    rest <- removeLeadingSemiColon rawRest
    op <- parseIntOperand internal
    return (Halt op, rest)
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
  where
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
    parseFactor [IdentifierTok name] = Right $ Identifier name
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
