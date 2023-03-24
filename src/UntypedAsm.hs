
module UntypedAsm
  ( Asm, assemble
  , pure, (>>=), (>>), return, mfix, fail
  , allocateZP, label, lo, hi
  , equb, equs
  , lda_i_char, lda_i, lda_iy, lda_iiy, ldy_i
  , sta_z
  , jmp, jsr, rts
  , beq, bne
  , iny
  ) where

import Prelude hiding (pure,(>>=),(>>),return,fail)
import Data.Bits (shiftR,(.&.))
import Data.ByteString.Internal (c2w)
import Data.Word (Word8,Word16)
import Assemble (Asm(..), assemble)

return :: v -> Asm v
pure :: v -> Asm v
(>>=) :: Asm v1 -> (v1 -> Asm v2) -> Asm v2
(>>) :: Asm () -> Asm v2 -> Asm v2
fail :: Asm v
mfix :: (v -> Asm v) -> Asm v
allocateZP :: Asm Word8
label :: Asm Word16
lo :: Word16 -> Word8
hi :: Word16 -> Word8
equb :: [Word8] -> Asm ()
equs :: String -> Asm ()
lda_i :: Word8 -> Asm ()
lda_i_char :: Char -> Asm ()
lda_iy :: Word16 -> Asm ()
lda_iiy :: Word8 -> Asm ()
ldy_i :: Word8 -> Asm ()
sta_z :: Word8 -> Asm ()
jmp :: Word16 -> Asm ()
jsr :: Word16 -> Asm ()
rts :: Asm ()
beq :: Word16 -> Asm ()
bne :: Word16 -> Asm ()
iny :: Asm ()

return = pure
pure = Pure
(>>=) = Bind
(>>) asm1 asm2 = asm1 >>= \() -> asm2
fail = error "UntypedAsm.fail"
mfix = Mfix
allocateZP = AllocateZP
label = Label
lo w = fromIntegral (w .&. 0xff)
hi w = fromIntegral (w `shiftR` 8)
equb = Emit
equs str = Emit (map c2w str)
lda_i b = Emit [0xa9, b]
lda_i_char c = Emit [0xa9, c2w c]
lda_iy a = Emit [0xb9, lo a, hi a]
lda_iiy b = Emit [0xb1, b]
ldy_i b = Emit [0xa0, b]
sta_z b = Emit [0x85, b]
jmp a = Emit [0x4c, lo a, hi a]
jsr a = Emit [0x20, lo a, hi a]
rts = Emit [0x60]
beq = branch 0xf0
bne = branch 0xd0
iny = Emit [0xc8]

branch :: Word8 -> Word16 -> Asm ()
branch opcode a =
  Label >>= \here -> Emit [opcode, fromIntegral (a - here - 2) ]
