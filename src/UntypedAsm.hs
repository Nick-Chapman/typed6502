
module UntypedAsm
  ( Asm, assemble
  , pure, (>>=), (>>), return, mfix, fail
  , Immediate, ZeroPage, MemAddr
  , allocateZP, label, lo, hi
  , equb, equs, immediate
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
allocateZP :: Asm ZeroPage
label :: Asm MemAddr
lo :: MemAddr -> Immediate
hi :: MemAddr -> Immediate
equb :: [Word8] -> Asm ()
equs :: String -> Asm ()
immediate :: Word8 -> Immediate
lda_i :: Immediate -> Asm ()
lda_i_char :: Char -> Asm ()
lda_iy :: MemAddr -> Asm ()
lda_iiy :: ZeroPage -> Asm ()
ldy_i :: Word8 -> Asm ()
sta_z :: ZeroPage -> Asm ()
jmp :: MemAddr -> Asm ()
jsr :: MemAddr -> Asm ()
rts :: Asm ()
beq :: MemAddr -> Asm ()
bne :: MemAddr -> Asm ()
iny :: Asm ()


return = pure
pure = Pure
(>>=) = Bind
(>>) asm1 asm2 = asm1 >>= \() -> asm2
fail = error "UntypedAsm.fail"
mfix = Mfix
allocateZP = AllocateZP >>= \b -> pure (ZeroPage b)
label = Label >>= \a -> pure (MemAddr a)
lo (MemAddr a) = Immediate (loByte a)
hi (MemAddr a) = Immediate (hiByte a)
equb = Emit
equs str = Emit (map c2w str)
immediate = Immediate
lda_i (Immediate b) = Emit [0xa9, b]
lda_i_char c = Emit [0xa9, c2w c]
lda_iy (MemAddr a) = Emit [0xb9, loByte a, hiByte a]
lda_iiy (ZeroPage b) = Emit [0xb1, b]
ldy_i b = Emit [0xa0, b]
sta_z (ZeroPage b) = Emit [0x85, b]
jmp (MemAddr a) = Emit [0x4c, loByte a, hiByte a]
jsr (MemAddr a) = Emit [0x20, loByte a, hiByte a]
rts = Emit [0x60]
beq = branch 0xf0
bne = branch 0xd0
iny = Emit [0xc8]

branch :: Word8 -> MemAddr -> Asm ()
branch opcode (MemAddr a) =
  Label >>= \here -> Emit [opcode, fromIntegral (a - here - 2) ]

data Immediate = Immediate Word8
newtype ZeroPage = ZeroPage Word8 deriving (Num)
newtype MemAddr = MemAddr Word16 deriving (Num)

loByte,hiByte :: Word16 -> Word8
loByte a = fromIntegral (a .&. 0xff)
hiByte a = fromIntegral (a `shiftR` 8)
