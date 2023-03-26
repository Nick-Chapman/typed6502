
module Goodbye (code) where

import Asm
import Data.Word (Word8)

code :: [Word8]
code = assemble 0x2000 $ Asm.mdo

  jmp main

  mytext <- label
  equs "Goodbye!\r"; equb [0]

  main <- label
  ldy_i 0

  loop <- label
  lda (IndexedY mytext)
  beq finished
  jsr osasci
  iny
  bne loop
  finished <- label

  spin <- label
  jmp spin

  where
    osasci = 0xffe3
