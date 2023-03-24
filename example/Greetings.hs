
module Greetings (code) where

import UntypedAsm as Asm
import Data.Word (Word8,Word16)

code :: [Word8]
code = assemble 0x2000 $ Asm.mdo

  ptr <- allocateZP

  jmp main

  hello <- label; equs "Hello!\r"; equb [0]
  goodbye <- label; equs "Goodbye!\r"; equb [0]

  main <- label
  copy16i hello ptr; jsr outputMessage
  copy16i goodbye ptr; jsr outputMessage

  spin <- label
  jmp spin

  outputMessage <- label
  ldy_i 0
  loop <- label
  lda_iiy ptr
  beq finished
  jsr osasci
  iny
  bne loop
  finished <- label
  rts

  where
    osasci = 0xffe3


copy16i :: Word16 -> Word8 -> Asm ()
copy16i a v = Asm.do
  lda_i (lo a) ; sta_z v
  lda_i (hi a) ; sta_z (v+1)
