module HSPC.CodeGen (generate) where

import Data.Binary.Put (putInt64le, runPut)
import Data.ByteString.Lazy qualified as LBS
import Data.Int (Int64)
import Data.Word (Word8)
import HSPC.ELF (getELFheader)
import HSPC.Parse (AST (..))

int64ToLE :: Int64 -> [Word8]
int64ToLE n = LBS.unpack $ runPut (putInt64le n)

generate :: AST -> [Word8]
generate ast =
  let code = generateProgram ast
   in getELFheader (fromIntegral (length code)) ++ code

generateProgram :: AST -> [Word8]
generateProgram (Program _ body) = concatMap generateProgram body
generateProgram (Block body) = concatMap generateProgram body
generateProgram (Halt ast) =
  generateProgram ast
    -- copy rax -> rdi but maxed out at 255
    ++ [0xbf, 0xff, 0, 0, 0] -- mov edi 255
    ++ [0x48, 0x3d, 0xff, 0, 0, 0] -- cmp rax 255
    ++ [0x48, 0x0f, 0x4c, 0xf8] -- cmovl rax rdi
    -- exit_group syscall, exit code is in rdi
    ++ [0xb8, 0x3c, 00, 00, 00] -- mov rax 60
    ++ [0x0f, 0x05] -- Syscall
generateProgram (IntLiteral i) =
  [0x48, 0xb8] ++ int64ToLE i -- movabs rax i
generateProgram (Add op1 op2) =
  generateProgram op1
    ++ [0x50] -- push rax
    ++ generateProgram op2
    ++ [0x48, 0x03, 0x04, 0x24] -- add rax [rsp]
    ++ [0x48, 0x83, 0xc4, 0x08] -- add rsp 8 (release stack space)
generateProgram (Multiply op1 op2) =
  generateProgram op1
    ++ [0x50] -- push rax
    ++ generateProgram op2
    ++ [0x48, 0x0f, 0xaf, 0x04, 0x24] -- imul rax [rsp]
    ++ [0x48, 0x83, 0xc4, 0x08] -- add rsp 8 (release stack space)
generateProgram (UnaryPlus op) = generateProgram op -- unary + is a nop
generateProgram (UnaryMinus op) =
  generateProgram op
    ++ [0x48, 0xf7, 0xd8]
generateProgram (IntDivide op1 op2) =
  generateProgram op2
    ++ [0x50] -- push rax
    ++ generateProgram op1
    ++ [0x48, 0x99] -- cqo
    ++ [0x48, 0xf7, 0x3c, 0x24] -- idiv qword [rsp]
    ++ [0x48, 0x83, 0xc4, 0x08] -- add rsp 8 (release stack space)
generateProgram ast = error $ "CodeGen for " ++ show ast ++ " not implemented"
