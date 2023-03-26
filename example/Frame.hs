
module Frame (code) where

import Prelude hiding (pure)
import Asm
import Data.Word (Word8)

copy16i :: MemAddr 'NotExecutable -> ZeroPage a -> Asm (State a1 x1 y1 s1) (State a2 x2 y2 s2) ()
copy16i a v = Asm.do
  lda (lo a) ; sta_z v
  lda (hi a) ; sta_z (v+1)

code :: [Word8]
code = assemble 0x2000 $ Asm.mdo

  msgPtr <- allocateZP

  let
    --puts :: String -> Asm 'NotExecutable 'NotExecutable ()
    puts str = Asm.mdo
      copy16i msg msgPtr
      jmp after
      msg <- label; equs str; equb [0]
      after <- label
      jsr printMessage

  jmp main
  --jmp main -- Is type error for unreachable code. Good

  frameCount <- label; equb [0]

  _mos_syncVB <- label
  lda @Word8 19
  jmp osbyte
  --rts -- TODO: detect bug

  _mode1 <- label
  lda @Word8 22 ; jsr oswrch
  lda @Word8 1 ; jsr oswrch
  rts

  _cursorOff <- label
  lda @Word8 23 ; jsr oswrch
  lda @Word8 1 ; jsr oswrch
  lda @Word8 0 ; jsr oswrch
  lda @Word8 0 ; jsr oswrch
  lda @Word8 0 ; jsr oswrch
  lda @Word8 0 ; jsr oswrch
  lda @Word8 0 ; jsr oswrch
  lda @Word8 0 ; jsr oswrch
  lda @Word8 0 ; jsr oswrch
  lda @Word8 0 ; jsr oswrch
  rts

  printMessage <- makePrintMessage msgPtr

  main <- label

  jsr _mode1
  jsr _cursorOff

  loop <- label
  jsr _mos_syncVB
  position 1 1; puts "Frame : "; lda (Absolute frameCount); jsr printHexA
  inc_m frameCount
  jmp loop

  printHexA <- makePrintHexA

  pure ()

makePrintHexA :: Asm g1 'NotExecutable (MemAddr (State a x y s))
makePrintHexA = Asm.mdo
  entry <- label
  pha ; lda '['; jsr osasci
  pla -- TODO: comment out to see bug
  pha
  and_i 0xf0
  lsr_a; lsr_a; lsr_a; lsr_a; tax
  lda (IndexedX digits)
  jsr osasci
  pla
  and_i 0x0f; tax
  lda (IndexedX digits)
  jsr osasci
  pha; lda ']'; jsr osasci; pla
  rts
  digits <- label
  equs "0123456789abcdef"
  pure entry

makePrintMessage :: ZeroPage v -> Asm g1 'NotExecutable (MemAddr (State a x o s))
makePrintMessage msgPtr = Asm.mdo
  entry <- label
  ldy_i 0
  loop <- label
  lda (IndirectY msgPtr)
  beq done
  jsr osasci
  iny
  bne loop
  done <- label
  rts
  pure entry

position :: Word8 -> Word8 -> Asm (State a1 x1 y1 s1) (State a2 x2 y2 s2) ()
position x y = Asm.do
  lda @Word8 31; jsr osasci
  lda x; jsr osasci
  lda y; jsr osasci

osasci,oswrch,osbyte :: MemAddr (State a1 x1 y1 s1)
osasci = 0xffe3
oswrch = 0xffee
osbyte = 0xfff4
