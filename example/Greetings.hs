
module Greetings (code) where

import Asm
import Data.Word (Word8)

code :: [Word8]
code = assemble 0x2000 $ Asm.mdo

  ptr <- allocateZP

  jmp main

  hello <- labelData; equs "Hello!\r"; equb [0]
  goodbye <- labelData; equs "Goodbye!\r"; equb [0]

  main <- labelEntry -- TEST: Bug if labelled as data
  copy16i hello ptr; jsr outputMessage
  copy16i goodbye ptr; jsr outputMessage

  spin <- labelCode
  jmp spin

  outputMessage <- labelEntry
  ldy_i 0
  loop <- labelCode
  lda (IndirectY ptr)
  beq finished
  jsr osasci
  iny
  bne loop
  finished <- labelCode
  rts

  where
    osasci = 0xffe3


copy16i :: MemAddr g -> ZpAddr a -> Asm g1 (State a x y s) ()
copy16i a v = Asm.do
  lda (lo a) ; sta_z v
  lda (hi a) ; sta_z (v+1)
