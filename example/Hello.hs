
module Hello (code) where

import SimpleAsm as Asm
import Data.Word (Word8)

code :: [Word8]
code = assemble 0x2000 $ Asm.mdo
  jmp main
  main <- label
  lda_i_char 'H'; jsr osasci
  lda_i_char 'e'; jsr osasci
  lda_i_char 'l'; jsr osasci; jsr osasci
  lda_i_char 'o'; jsr osasci
  lda_i_char '!'; jsr osasci
  lda_i_char '\r'; jsr osasci --13
  spin <- label
  jmp spin

  where
    osasci = 0xffe3
