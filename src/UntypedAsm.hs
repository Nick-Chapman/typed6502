
module UntypedAsm
  ( Asm, assemble
  , (>>=), (>>), return, pure, mfix, fail
  , ZeroPage, MemAddr
  , allocateZP, label, lo, hi, equb, equs
  , IndexedX(..)
  , IndexedY(..)
  , IndirectY(..)

  , and_i
  , beq
  , bne
  , inc_m
  , iny
  , jmp
  , jsr
  , lda_i
  , lda_i_char
  , lda_iiy
  , lda_iy
  , lda_m
  , lda_mx
  , ldy_i
  , lsr_a
  , pha
  , pla
  , rts
  , sta_z
  , tax

  ) where

import Prelude hiding (pure,(>>=),(>>),return,fail)
import Data.Bits (shiftR,(.&.))
import Data.ByteString.Internal (c2w)
import Data.Word (Word8,Word16)
import Assemble (Asm(..), assemble)

(>>=) :: Asm v1 -> (v1 -> Asm v2) -> Asm v2
(>>) :: Asm () -> Asm v2 -> Asm v2
return :: v -> Asm v
pure :: v -> Asm v
mfix :: (v -> Asm v) -> Asm v
fail :: Asm v

newtype ZeroPage = ZeroPage Word8 deriving (Num)
newtype MemAddr = MemAddr Word16 deriving (Num)

newtype IndexedX = IndexedX MemAddr
newtype IndexedY = IndexedY MemAddr
newtype IndirectY = IndirectY ZeroPage

allocateZP :: Asm ZeroPage
label :: Asm MemAddr
lo :: MemAddr -> Word8
hi :: MemAddr -> Word8
equb :: [Word8] -> Asm ()
equs :: String -> Asm ()

and_i :: Word8 -> Asm ()
beq :: MemAddr -> Asm ()
bne :: MemAddr -> Asm ()
inc_m :: MemAddr -> Asm ()
iny :: Asm ()
jmp :: MemAddr -> Asm ()
jsr :: MemAddr -> Asm ()

lda_i :: Word8 -> Asm ()
lda_i_char :: Char -> Asm ()
lda_iiy :: IndirectY -> Asm ()
lda_iy :: IndexedY -> Asm ()
lda_m :: MemAddr -> Asm ()
lda_mx :: IndexedX -> Asm ()

ldy_i :: Word8 -> Asm ()
lsr_a :: Asm ()
pha :: Asm ()
pla :: Asm ()
rts :: Asm ()
sta_z :: ZeroPage -> Asm ()
tax :: Asm ()


(>>=) = Bind
(>>) asm1 asm2 = asm1 >>= \() -> asm2
return = Pure
pure = Pure
mfix = Mfix
fail = error "UntypedAsm.fail"

allocateZP = AllocateZP >>= \b -> pure (ZeroPage b)
label = Label >>= \a -> pure (MemAddr a)
lo (MemAddr a) = loByte a
hi (MemAddr a) = hiByte a
equb = Emit
equs str = Emit (map c2w str)

and_i = op1 0x29
beq = branch 0xf0
bne = branch 0xd0
inc_m (MemAddr a) = op2 0xee a
iny = op0 0xc8
jmp (MemAddr a) = op2 0x4c a
jsr (MemAddr a) = op2 0x20 a
lda_i = op1 0xa9
lda_i_char c = lda_i (c2w c)
lda_iiy (IndirectY (ZeroPage b)) = op1 0xb1 b
lda_iy (IndexedY (MemAddr a)) = op2 0xb9 a
lda_m (MemAddr a) = op2 0xad a
lda_mx (IndexedX (MemAddr a)) = op2 0xbd a
ldy_i = op1 0xa0
lsr_a = op0 0x4a
pha = op0 0x48
pla = op0 0x68
rts = op0 0x60
sta_z (ZeroPage b) = op1 0x85 b
tax = op0 0xaa


branch :: Word8 -> MemAddr -> Asm ()
branch opcode (MemAddr a) =
  Label >>= \here -> op1 opcode (fromIntegral (a - here - 2))

op0 :: Word8 -> Asm ()
op0 code = Emit [code]

op1 :: Word8 -> Word8 -> Asm ()
op1 code b = Emit [code, b]

op2 :: Word8 -> Word16 -> Asm ()
op2 code a = Emit [code, loByte a, hiByte a]

loByte,hiByte :: Word16 -> Word8
loByte a = fromIntegral (a .&. 0xff)
hiByte a = fromIntegral (a `shiftR` 8)
