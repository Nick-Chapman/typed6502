
module Hello (code) where

import SimpleAsm as Asm
import Data.Word (Word8)

code :: [Word8]
code = assemble 0x2000 $ Asm.mdo
  jmp main
  main <- label
  lda 'H'; jsr osasci
  lda 'e'; jsr osasci
  lda 'l'; jsr osasci; jsr osasci
  lda 'o'; jsr osasci
  lda '!'; jsr osasci
  lda '\r'; jsr osasci --13
  spin <- label
  jmp spin

  where
    osasci = 0xffe3
