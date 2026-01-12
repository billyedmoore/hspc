-- Get x86-64 ELF Header
-- Adapted from https://github.com/billyedmoore/brainf-ck
module HSPC.ELF (getELFheader) where

import Data.Bits (shiftR)
import Data.Word (Word64, Word8)

getELFheader :: Word64 -> [Word8]
getELFheader programSize = elfHeader ++ programHeader (120 + programSize)

word64ToLE :: Word64 -> [Word8]
word64ToLE w = [fromIntegral (w `shiftR` (8 * i)) | i <- [0 .. 7]]

elfHeader :: [Word8]
elfHeader =
  [0x7F, 0x45, 0x4C, 0x46] -- ELF
    ++ [0x2] -- (e_ident[EI_CLASS]) 64 bit
    ++ [0x1] -- (e_ident[DATA]) little endian
    ++ [0x1] -- (e_ident[EI_VERSION]) ELF v1
    ++ [0x0] -- (e_ident[EI_OSABI]) System v (aparently this is used rather than linux)
    ++ [0, 0, 0, 0, 0, 0, 0, 0] -- (e_ident[EI_PAD]) Padding
    ++ [0x2, 0] -- (e_type) Executable
    ++ [0x3E, 0] -- (e_machine) x84-64
    ++ [0x1, 0, 0, 0] -- (e_version) ELF v1
    ++ [0x78, 0, 0x40, 0, 0, 0, 0, 0] -- (e_entry) program entry point
    ++ [0x40, 0, 0, 0, 0, 0, 0, 0] -- (e_phoff) program header location
    ++ [0, 0, 0, 0, 0, 0, 0, 0] -- (e_shoff) start of the section header table??
    ++ [0, 0, 0, 0] -- (e_flags)
    ++ [0x40, 0] -- (e_ehsize) size of this header (64 bytes)
    ++ [0x38, 0] -- (e_phentsize) size of the program header table
    ++ [1, 0] -- (e_phnum) one program header
    ++ [0x40, 0] -- (e_shentsize) section header size (64 bytes)
    ++ [0, 0] -- (e_shnum) no section headers
    ++ [0, 0] -- (e_shstrndx) index of section names (we dont have any)

programHeader :: Word64 -> [Word8]
programHeader fileSize =
  [0x1, 0, 0, 0] -- (p_type) PT_LOAD
    ++ [0x7, 0, 0, 0] -- (p_flags) read and execute
    ++ [0, 0, 0, 0, 0, 0, 0, 0] -- (p_offset)
    ++ [0, 0, 0x40, 0, 0, 0, 0, 0] -- (p_vaddr)
    ++ [0, 0, 0, 0, 0, 0, 0, 0] -- (p_paddr)
    ++ word64ToLE fileSize -- (p_filesz)
    ++ word64ToLE fileSize -- (p_memsz)
    ++ [0, 0, 0, 0, 0, 0, 0, 0] -- (p_align) program alignment?
