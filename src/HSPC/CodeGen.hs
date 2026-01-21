module HSPC.CodeGen (generate) where

import Data.Binary.Put (putInt32le, putInt64le, runPut)
import Data.ByteString.Lazy qualified as LBS
import Data.Int (Int32, Int64)
import Data.List (mapAccumL)
import Data.Map qualified as Map
import Data.Word (Word8)
import HSPC.ELF (getELFheader)
import HSPC.Parse (Block (MainProgramBlock), Expression (..), HSPCDataType (..), Program (..), Statement (..))

type OffsetMap = Map.Map String Int32

dataTypeToSize :: HSPCDataType -> Int
dataTypeToSize IntegerType = 64
-- space efficency is clearly a top priority
dataTypeToSize BooleanType = 64

buildOffsetMap :: [(String, HSPCDataType)] -> (Int32, OffsetMap)
buildOffsetMap vars = (totalSize, Map.fromList mappedVars)
  where
    (totalSize, mappedVars) = mapAccumL calculateOffset 0 vars

    calculateOffset :: Int32 -> (String, HSPCDataType) -> (Int32, (String, Int32))
    calculateOffset currentDepth (name, dtype) =
      let size = (fromIntegral . dataTypeToSize) dtype
          newDepth = currentDepth + size
          rbpOffset = -newDepth
       in (newDepth, (name, rbpOffset))

int64ToLE :: Int64 -> [Word8]
int64ToLE n = LBS.unpack $ runPut (putInt64le n)

int32ToLE :: Int32 -> [Word8]
int32ToLE n = LBS.unpack $ runPut (putInt32le n)

generate :: Program -> [Word8]
generate ast =
  let code = generateProgram ast
   in getELFheader (fromIntegral (length code)) ++ code

generateProgram :: Program -> [Word8]
generateProgram (Program _ body) = concatMap generateBlock body

generateBlock :: Block -> [Word8]
generateBlock (MainProgramBlock vars body) =
  let (size, offsetMap) = buildOffsetMap vars
      leSize = int32ToLE size
   in [0x55] -- push rpb
        ++ [0x48, 0x89, 0xe5] -- mov rbp rsp
        ++ [0x48, 0x81, 0xec]
        ++ leSize -- sub rsb size
        ++ concatMap (generateStatement offsetMap) body
        ++ [0x48, 0x81, 0xc4]
        ++ leSize -- add rsb size
        ++ [0x5d] -- pop rbp

generateStatement :: OffsetMap -> Statement -> [Word8]
generateStatement offsetMap (Halt ast) =
  generateExpression offsetMap ast
    -- copy rax -> rdi but maxed out at 255
    ++ [0xbf, 0xff, 0, 0, 0] -- mov edi 255
    ++ [0x48, 0x3d, 0xff, 0, 0, 0] -- cmp rax 255
    ++ [0x48, 0x0f, 0x4c, 0xf8] -- cmovl rax rdi
    -- exit_group syscall, exit code is in rdi
    ++ [0xb8, 0x3c, 00, 00, 00] -- mov rax 60
    ++ [0x0f, 0x05] -- Syscall
generateStatement offsetMap (Assignment name op) =
  generateExpression offsetMap op
    ++ case Map.lookup name offsetMap of
      Just offset -> [0x48, 0x89, 0x85] ++ int32ToLE offset -- mov [rbp + offset] rax
      Nothing -> error $ "Variable " ++ name ++ " doesn't exist."
generateStatement _ ast = error $ "CodeGen for " ++ show ast ++ " not implemented"

generateExpression :: OffsetMap -> Expression -> [Word8]
generateExpression _ (IntLiteral i) = [0x48, 0xb8] ++ int64ToLE i -- movabs rax i
generateExpression _ (BoolLiteral b) =
  [0x48, 0xb8]
    ++ (int64ToLE . fromIntegral . fromEnum) b -- movabs rax i
generateExpression offsetMap (Identifier str) =
  case Map.lookup str offsetMap of
    Just offset -> [0x48, 0x8b, 0x85] ++ int32ToLE offset -- mov rax [rbp - n]
    Nothing -> error $ "Variable " ++ str ++ " doesn't exist."
generateExpression offsetMap (Add op1 op2) =
  generateExpression offsetMap op1
    ++ [0x50] -- push rax
    ++ generateExpression offsetMap op2
    ++ [0x48, 0x03, 0x04, 0x24] -- add rax [rsp]
    ++ [0x48, 0x83, 0xc4, 0x08] -- add rsp 8 (release stack space)
generateExpression offsetMap (Multiply op1 op2) =
  generateExpression offsetMap op1
    ++ [0x50] -- push rax
    ++ generateExpression offsetMap op2
    ++ [0x48, 0x0f, 0xaf, 0x04, 0x24] -- imul rax [rsp]
    ++ [0x48, 0x83, 0xc4, 0x08] -- add rsp 8 (release stack space)
generateExpression offsetMap (UnaryPlus op) = generateExpression offsetMap op -- unary + is a nop
generateExpression offsetMap (UnaryMinus op) =
  generateExpression offsetMap op
    ++ [0x48, 0xf7, 0xd8]
generateExpression offsetMap (IntDivide op1 op2) =
  generateExpression offsetMap op2
    ++ [0x50] -- push rax
    ++ generateExpression offsetMap op1
    ++ [0x48, 0x99] -- cqo
    ++ [0x48, 0xf7, 0x3c, 0x24] -- idiv qword [rsp]
    ++ [0x48, 0x83, 0xc4, 0x08] -- add rsp 8 (release stack space)
generateExpression _ ast = error $ "CodeGen for " ++ show ast ++ " not implemented"
