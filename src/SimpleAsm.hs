
module SimpleAsm
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

  , lda

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

class Lda arg where lda :: arg -> Asm ()

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

instance Lda Word8 where lda = op1 0xa9
instance Lda Char where lda c = lda (c2w c)
instance Lda IndirectY where lda (IndirectY (ZeroPage b)) = op1 0xb1 b
instance Lda IndexedY where lda (IndexedY (MemAddr a)) = op2 0xb9 a
instance Lda IndexedX where lda (IndexedX (MemAddr a)) = op2 0xbd a
instance Lda MemAddr where lda (MemAddr a) = op2 0xad a

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
