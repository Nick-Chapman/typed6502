
module Bf (code) where

import Prelude hiding (pure)
import Asm
import Data.Word (Word8)

code :: [Word8]
code = assemble 0x2000 $ Asm.mdo

  mp <- allocateZP
  _ <- allocateZP
  ip <- allocateZP
  _ <- allocateZP

  lda (lo prog)
  sta_z ip
  lda (hi prog)
  sta_z (ip+1)
  lda (lo mem)
  sta_z mp
  lda (hi mem)
  sta_z (mp+1)
  ldy_i (immediate 0)
  lda (immediate 0)
  ldx_i (immediate 0xff)

  zeromem <- labelCode
  sta (IndexedX mem)
  dex
  bne zeromem
  sta (Absolute mem)
  jmp fetch

  advance <- labelEntry
  jsr incip

  fetch <- labelCode
  lda (IndirectY ip)
  bne left
  rts

  left <- label
  cmp_c '<' ; bne right
  dec_z mp
  jmp advance

  right <- label
  cmp_c '>' ; bne plus
  inc_z mp
  jmp advance

  plus <- label
  cmp_c '+' ; bne minus
  lda (IndirectY mp)
  clc
  adc (immediate 1)
  sta (IndirectY mp)
  jmp advance

  minus <- label
  cmp_c '-' ; bne comma
  lda (IndirectY mp)
  sec
  sbc (immediate 1)
  sta (IndirectY mp)
  jmp advance

  comma <- label
  cmp_c ',' ; bne dot
  jsr osrdch
  sta (IndirectY mp)
  jmp advance

  dot <- label
  cmp_c '.' ; bne open
  lda (IndirectY mp)
  cmp_c '\n' ; bne print
  clc
  adc (immediate 3)

  print <- label
  jsr oswrch
  jmp advance

  open <- label
  cmp_c '[' ; bne close
  lda (IndirectY mp)
  bne advance
  ldx_i (immediate 1)

  forward <- label
  jsr incip
  lda (IndirectY ip)
  cmp_c '[' ; bne forward2
  inx
  jmp forward

  forward2 <- label
  cmp_c ']' ; bne forward
  dex
  bne forward
  jmp advance

  close <- label

  cmp_c ']' ; bne advance
  lda (IndirectY mp)
  beq done
  ldx_i (immediate 1)

  backward <- label
  jsr decip
  lda (IndirectY ip)
  cmp_c ']' ; bne backward2
  inx
  jmp backward

  backward2 <- label
  cmp_c '[' ; bne backward
  dex
  bne backward
  done <- label
  jmp advance

  incip <- label
  inc_z ip
  lda (ZeroPage ip)
  bne incip2
  inc_z (ip+1)
  incip2 <- label
  rts

  decip <- label
  lda (ZeroPage ip)
  bne decip2
  dec_z (ip+1)
  decip2 <- label
  dec_z ip
  rts

  prog <- label --Data -- TODO: use labels with intent
  equs ">++++++++++>+>+[[+++++[>++++++++<-]>.<++++++[>--------<-]+<<<]>.>>[[-]<[>+<-]>>[<<+>+>-]<[>+<-[>+<-[>+<-[>+<-[>+<-[>+<- [>+<-[>+<-[>+<-[>[-]>+>+<<<-[>+<-]]]]]]]]]]]+>>>]<<<]"
  equb [0]

  mem <- labelData
  pure ()

osrdch :: MemAddr a -- TODO: type to capture these routines
osrdch = 0xffe0

oswrch :: MemAddr a
oswrch = 0xffe3
