
module Hello (code) where

import Asm
import Data.Word (Word8)

code :: [Word8]
code = assemble 0x2000 $ Asm.mdo
  jmp main
  main <- labelEntry
  ldaIC 'H'; jsr osasci
  ldaIC 'e'; jsr osasci
  ldaIC 'l'; jsr osasci; jsr osasci
  ldaIC 'o'; jsr osasci
  ldaIC '!'; jsr osasci
  ldaIC '\r'; jsr osasci --13
  spin <- labelCode
  jmp spin

  where
    osasci = 0xffe3
    ldaIC = lda . immChar
