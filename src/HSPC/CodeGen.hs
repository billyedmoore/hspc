module HSPC.CodeGen (generate) where

import Data.Word (Word8)
import HSPC.ELF (getELFheader)
import HSPC.Parse (AST (..))

generate :: AST -> [Word8]
generate ast =
  let code = generateProgram ast
   in getELFheader (fromIntegral (length code)) ++ code

generateProgram :: AST -> [Word8]
generateProgram (Program _ body) = concatMap generateProgram body
generateProgram (Block body) = concatMap generateProgram body
generateProgram (Halt i) =
  [0xb8, 0x3c, 00, 00, 00] -- mov eax 60 (sys_exit number)
    ++ [0xbf, fromIntegral i, 00, 00, 00] -- Set exit status to i
    ++ [0x0f, 0x05] -- Syscall
generateProgram _ = []
