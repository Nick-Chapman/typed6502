
module Frame (code) where

import Prelude hiding (pure)
import SimpleAsm as Asm
import Data.Word (Word8)

code :: [Word8]
code = assemble 0x2000 $ Asm.mdo

  msgPtr <- allocateZP

  let
    puts :: String -> Asm ()
    puts str = Asm.mdo
      copy16i msg msgPtr
      jmp after
      msg <- label; equs str; equb [0]
      after <- label
      jsr printMessage

  jmp main

  frameCount <- label; equb [0]

  _mos_syncVB <- label
  lda_i 19
  jmp osbyte
  --rts -- TODO: detect bug

  _mode1 <- label
  lda_i 22 ; jsr oswrch
  lda_i 1 ; jsr oswrch
  rts

  _cursorOff <- label
  lda_i 23 ; jsr oswrch
  lda_i 1 ; jsr oswrch
  lda_i 0 ; jsr oswrch
  lda_i 0 ; jsr oswrch
  lda_i 0 ; jsr oswrch
  lda_i 0 ; jsr oswrch
  lda_i 0 ; jsr oswrch
  lda_i 0 ; jsr oswrch
  lda_i 0 ; jsr oswrch
  lda_i 0 ; jsr oswrch
  rts

  printMessage <- makePrintMessage msgPtr

  main <- label

  jsr _mode1
  jsr _cursorOff

  loop <- label
  jsr _mos_syncVB
  position 1 1; puts "Frame : "; lda_m frameCount; jsr printHexA
  inc_m frameCount
  jmp loop

  printHexA <- makePrintHexA

  pure ()

makePrintHexA :: Asm MemAddr
makePrintHexA = Asm.mdo
  entry <- label
  pha ; lda_i_char '['; jsr osasci; pla
  pha
  and_i 0xf0
  lsr_a; lsr_a; lsr_a; lsr_a; tax
  lda_mx (IndexedX digits)
  jsr osasci
  pla
  and_i 0x0f; tax
  lda_mx (IndexedX digits)
  jsr osasci
  pha; lda_i_char ']'; jsr osasci; pla
  rts
  digits <- label
  equs "0123456789abcdef"
  pure entry

makePrintMessage :: ZeroPage -> Asm MemAddr
makePrintMessage msgPtr = Asm.mdo
  entry <- label
  ldy_i 0
  loop <- label
  lda_iiy (IndirectY msgPtr)
  beq done
  jsr osasci
  iny
  bne loop
  done <- label
  rts
  pure entry

position :: Word8 -> Word8 -> Asm ()
position x y = Asm.do
  lda_i 31; jsr osasci
  lda_i x; jsr osasci
  lda_i y; jsr osasci

copy16i :: MemAddr -> ZeroPage -> Asm ()
copy16i a v = Asm.do
  lda_i (lo a) ; sta_z v
  lda_i (hi a) ; sta_z (v+1)

osasci,oswrch,osbyte :: MemAddr
osasci = 0xffe3
oswrch = 0xffee
osbyte = 0xfff4
