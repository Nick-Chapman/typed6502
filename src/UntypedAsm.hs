
module UntypedAsm
  ( Asm, assemble
  , pure, (>>=), (>>), return, mfix, fail
  , ZeroPage, MemAddr
  , allocateZP, label, lo, hi
  , equb, equs
  , lda_i, lda_i_char , lda_m, lda_mx , lda_iy, lda_iiy
  , and_i
  , ldy_i
  , sta_z
  , jmp, jsr, rts
  , beq, bne
  , inc_m, iny, tax, lsr_a
  , pha, pla
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
lo :: MemAddr -> Word8
hi :: MemAddr -> Word8
equb :: [Word8] -> Asm ()
equs :: String -> Asm ()
lda_i :: Word8 -> Asm ()
lda_i_char :: Char -> Asm ()
lda_m :: MemAddr -> Asm ()
lda_mx :: MemAddr -> Asm ()
lda_iy :: MemAddr -> Asm ()
lda_iiy :: ZeroPage -> Asm ()
and_i :: Word8 -> Asm ()
ldy_i :: Word8 -> Asm ()
sta_z :: ZeroPage -> Asm ()
jmp :: MemAddr -> Asm ()
jsr :: MemAddr -> Asm ()
rts :: Asm ()
beq :: MemAddr -> Asm ()
bne :: MemAddr -> Asm ()
inc_m :: MemAddr -> Asm ()
iny :: Asm ()
tax :: Asm ()
lsr_a :: Asm ()
pha :: Asm ()
pla :: Asm ()


return = pure
pure = Pure
(>>=) = Bind
(>>) asm1 asm2 = asm1 >>= \() -> asm2
fail = error "UntypedAsm.fail"
mfix = Mfix
allocateZP = AllocateZP >>= \b -> pure (ZeroPage b)
label = Label >>= \a -> pure (MemAddr a)
lo (MemAddr a) = loByte a
hi (MemAddr a) = hiByte a
equb = Emit
equs str = Emit (map c2w str)
lda_i = op1 0xa9
lda_i_char c = op1 0xa9 (c2w c)
lda_m (MemAddr a) = op2 0xad a
lda_mx (MemAddr a) = op2 0xbd a
lda_iy (MemAddr a) = op2 0xb9 a
lda_iiy (ZeroPage b) = op1 0xb1 b
and_i = op1 0x29
ldy_i = op1 0xa0
sta_z (ZeroPage b) = op1 0x85 b
jmp (MemAddr a) = op2 0x4c a
jsr (MemAddr a) = op2 0x20 a
rts = op0 0x60
beq = branch 0xf0
bne = branch 0xd0
inc_m (MemAddr a) = op2 0xee a
iny = op0 0xc8
lsr_a = op0 0x4a
tax = op0 0xaa
pha = op0 0x48
pla = op0 0x68


branch :: Word8 -> MemAddr -> Asm ()
branch opcode (MemAddr a) =
  Label >>= \here -> op1 opcode (fromIntegral (a - here - 2))

op0 :: Word8 -> Asm ()
op0 code = Emit [code]

op1 :: Word8 -> Word8 -> Asm ()
op1 code b = Emit [code, b]

op2 :: Word8 -> Word16 -> Asm ()
op2 code a = Emit [code, loByte a, hiByte a]

newtype ZeroPage = ZeroPage Word8 deriving (Num)
newtype MemAddr = MemAddr Word16 deriving (Num)

loByte,hiByte :: Word16 -> Word8
loByte a = fromIntegral (a .&. 0xff)
hiByte a = fromIntegral (a `shiftR` 8)
