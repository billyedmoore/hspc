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

data HSPCDataType
  = IntegerType
  | BooleanType
  deriving (Show, Eq)

data Block
  = -- Block locals body
    MainProgramBlock [(String, HSPCDataType)] [Statement]
  deriving (Show, Eq)

data Statement
  = VarAssign String Expression
  | If Expression Statement Statement
  | While Expression Statement
  | NOP
  | Assignment String Expression
  | Halt Expression
  | ExprStmt Expression
  deriving (Show, Eq)

data Expression
  = Identifier String
  | IntLiteral Int64
  | BoolLiteral Bool
  | BoolNot Expression
  | BoolAnd Expression Expression
  | BoolOr Expression Expression
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
  | VariableNotDeclared String
  | InvalidDataType HSPCToken
  | UnexpectedToken HSPCToken
  | ExpectedToken HSPCToken
  | NotImplemented String [HSPCToken]
  deriving (Show)

toDataType :: HSPCToken -> Either ParseError HSPCDataType
toDataType IntegerTypeTok = Right IntegerType
toDataType BooleanTypeTok = Right BooleanType
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
  (expr, restWithSemiColon) <- parseStatement xs
  rest <- removeLeadingSemiColon restWithSemiColon
  parseStatements (expr : acc) rest

parseStatement :: [HSPCToken] -> Either ParseError (Statement, [HSPCToken])
parseStatement (IdentifierTok name : AssignmentTok : xs) = do
  let (rhs, rest) = break (`elem` [SemiColonTok, ElseKeyWordTok]) xs
  op <- parseExpression rhs
  return (Assignment name op, rest)
parseStatement
  (HaltBuiltInTok : OpenBracketTok : CloseBracketTok : xs) = Right (Halt (IntLiteral 0), xs)
parseStatement
  (HaltBuiltInTok : OpenBracketTok : xs) = do
    (internal, rest) <- splitToMatchingBracket xs
    op <- parseExpression internal
    return (Halt op, rest)

-- "IF cond THEN statement (; | ELSE elseStatement;)"
-- ELSE IF is simply an if statement where elseSatement
-- is another if statement.
parseStatement (IfKeyWordTok : xs) = do
  (cond, afterThen) <- parseIfCondition xs
  (statement, afterStatement) <- parseStatement afterThen
  case afterStatement of
    (SemiColonTok : outsideOfStatement) ->
      return (If cond statement NOP, SemiColonTok : outsideOfStatement)
    (ElseKeyWordTok : afterElse) -> do
      (elseStatement, afterElseStatement) <- parseStatement afterElse
      outsideOfStatement <- removeLeadingSemiColon afterElseStatement
      return (If cond statement elseStatement, SemiColonTok : outsideOfStatement)
    _ -> Left $ ExpectedToken SemiColonTok
  where
    parseIfCondition :: [HSPCToken] -> Either ParseError (Expression, [HSPCToken])
    parseIfCondition = parseCondition ThenKeyWordTok
parseStatement (WhileKeyWordTok : xs) = do
  (cond, afterThen) <- parseWhileCondition xs
  (statement, afterStatement) <- parseStatement afterThen
  outsideOfStatement <- removeLeadingSemiColon afterStatement
  return (While cond statement, SemiColonTok : outsideOfStatement)
  where
    parseWhileCondition :: [HSPCToken] -> Either ParseError (Expression, [HSPCToken])
    parseWhileCondition = parseCondition DoKeyWordTok
parseStatement tok = Left $ NotImplemented "parseStatement" tok

parseCondition :: HSPCToken -> [HSPCToken] -> Either ParseError (Expression, [HSPCToken])
parseCondition doneTok tokens = do
  let (condTokens, afterCond) = break (== doneTok) tokens
  cond <- parseExpression condTokens
  return (cond, drop 1 afterCond)

-- BoolTerm     = BoolFactor { "and" BoolFactor }
-- BoolFactor   = "not" BoolFactor | Relation
-- Relation     = SimpleExpr [ RelOp SimpleExpr ]
-- SimpleExpr   = Term { AddOp Term }
-- Term         = Factor { MulOp Factor }
-- Factor       = + Factor | - Factor | Integer | True | False | "(" Expression ")"
-- RelOp        = "==" | "!=" | "<" | ">" | "<=" | ">="
-- AddOp        = "+" | "-"
-- MulOp        = "*" | "/"
parseExpression :: [HSPCToken] -> Either ParseError Expression
parseExpression exprTokens = do
  (result, remaining) <- parseExpressionInternal exprTokens
  case remaining of
    [] -> Right result
    (tok : _) -> Left $ UnexpectedToken tok
  where
    parseExpressionInternal :: [HSPCToken] -> Either ParseError (Expression, [HSPCToken])
    parseExpressionInternal toks = do
      (left, rest) <- parseBoolTerm toks
      loopOr left rest
      where
        loopOr acc (OrTok : xs) = do
          (right, rest') <- parseBoolTerm xs
          loopOr (BoolOr acc right) rest'
        loopOr acc tokens = Right (acc, tokens)

    parseBoolTerm :: [HSPCToken] -> Either ParseError (Expression, [HSPCToken])
    parseBoolTerm toks = do
      (left, rest) <- parseBoolFactor toks
      loopAnd left rest
      where
        loopAnd acc (AndTok : xs) = do
          (right, rest') <- parseBoolFactor xs
          loopAnd (BoolAnd acc right) rest'
        loopAnd acc tokens = Right (acc, tokens)

    parseBoolFactor :: [HSPCToken] -> Either ParseError (Expression, [HSPCToken])
    parseBoolFactor (NotTok : xs) = do
      (inner, rest) <- parseBoolFactor xs
      return (BoolNot inner, rest)
    parseBoolFactor toks = parseRelation toks

    -- TODO: parse relations such as ==, >=, <
    parseRelation :: [HSPCToken] -> Either ParseError (Expression, [HSPCToken])
    parseRelation = parseSimpleExpr

    parseSimpleExpr :: [HSPCToken] -> Either ParseError (Expression, [HSPCToken])
    parseSimpleExpr toks = do
      (left, rest) <- parseTerm toks
      loopAdd left rest
      where
        loopAdd acc (PlusTok : xs) = do
          (right, rest') <- parseTerm xs
          loopAdd (Add acc right) rest'
        loopAdd acc (MinusTok : xs) = do
          (right, rest') <- parseTerm xs
          loopAdd (Subtract acc right) rest'
        loopAdd acc tokens = Right (acc, tokens)

    parseTerm :: [HSPCToken] -> Either ParseError (Expression, [HSPCToken])
    parseTerm toks = do
      (left, rest) <- parseFactor toks
      loopMul left rest
      where
        loopMul acc (MultiplyTok : xs) = do
          (right, rest') <- parseFactor xs
          loopMul (Multiply acc right) rest'
        loopMul acc (IntDivideTok : xs) = do
          (right, rest') <- parseFactor xs
          loopMul (IntDivide acc right) rest'
        loopMul acc tokens = Right (acc, tokens)

    parseFactor :: [HSPCToken] -> Either ParseError (Expression, [HSPCToken])
    parseFactor (PlusTok : xs) = do
      (inner, rest) <- parseFactor xs
      return (UnaryPlus inner, rest)
    parseFactor (MinusTok : xs) = do
      (inner, rest) <- parseFactor xs
      return (UnaryMinus inner, rest)
    parseFactor (LiteralIntTok n : xs) = Right (IntLiteral n, xs)
    parseFactor (LiteralBoolTok b : xs) = Right (BoolLiteral b, xs)
    parseFactor (IdentifierTok name : xs) = Right (Identifier name, xs)
    parseFactor (OpenBracketTok : xs) = do
      (expr, rest) <- parseExpressionInternal xs
      case rest of
        (CloseBracketTok : final) -> Right (expr, final)
        [] -> Left $ ExpectedOperand "Expected closing bracket ')'"
        (tok : _) -> Left $ UnexpectedToken tok
    parseFactor [] = Left $ ExpectedOperand "Expected operand"
    parseFactor (tok : _) = Left $ UnexpectedToken tok
