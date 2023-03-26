
module CharX (code) where

import Asm
import Data.Word (Word8)

code :: [Word8]
code = assemble 0x2000 $ Asm.do
  lda 'X'
  jsr osasci
  spin <- label
  jmp spin

  where
    osasci = 0xffe3
