
module Goodbye (code) where

import Prelude hiding (pure)
import UntypedAsm as Asm
import Data.Word (Word8)

code :: [Word8]
code = assemble 0x2000 $ Asm.mdo

  _addr <- allocateZP

  jmp main

  mytext <- label
  equs "Goodbye!\r"; equb [0]

  main <- label
  --lda_i (lo mytext) ; sta_z addr
  --lda_i (hi mytext) ; sta_z (addr+1)

  ldy_i 0

  loop <- label
  lda_my mytext
  beq finished
  jsr osasci
  iny
  bne loop
  finished <- label

  spin <- label
  jmp spin

  where
    osasci = 0xffe3
