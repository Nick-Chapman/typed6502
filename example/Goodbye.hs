
module Goodbye (code) where

import Asm
import Data.Word (Word8)

code :: [Word8]
code = assemble 0x2000 $ Asm.mdo

  jmp main

  mytext <- labelData
  equs "Goodbye!\r"; equb [0]

  main <- labelEntry
  ldy (immediate 0)

  loop <- labelCode
  lda (IndexedY mytext)
  beq finished
  jsr osasci
  iny
  bne loop
  finished <- labelCode

  spin <- labelCode
  jmp spin

  where
    osasci = 0xffe3
