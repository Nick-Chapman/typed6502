
module SimpleAsm
  ( Asm0,  Asm, VAL(..), STACK(..), CPU(..), GENERATED(..), State
  , assemble
  , (>>=), (>>), return, pure, mfix, fail
  , ZeroPage0, ZeroPage
  , MemAddr0, MemAddr
  , allocateZP, label, lo, hi, equb, equs
  , Absolute(..)
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

import Phantom
import Prelude hiding (pure,(>>=),(>>),return,fail)
import Data.Bits (shiftR,(.&.))
import Data.ByteString.Internal (c2w)
import Data.Word (Word8,Word16)
import qualified Assemble (Asm(..))
import Assemble (assemble)

type Asm0 = Assemble.Asm
type Asm (g :: GENERATED) (h :: GENERATED) = Asm0

(>>=) :: Asm0 v1 -> (v1 -> Asm0 v2) -> Asm0 v2
(>>) :: Asm0 () -> Asm0 v2 -> Asm0 v2
return :: v -> Asm0 v
pure :: v -> Asm0 v
mfix :: (v -> Asm0 v) -> Asm0 v
fail :: Asm0 v

newtype ZeroPage0 = ZeroPage Word8 deriving (Num)
newtype MemAddr0 = MemAddr Word16 deriving (Num)

type ZeroPage (a :: VAL) = ZeroPage0
type MemAddr (a :: GENERATED) = MemAddr0

newtype Absolute = Absolute MemAddr0
newtype IndexedX = IndexedX MemAddr0
newtype IndexedY = IndexedY MemAddr0
newtype IndirectY = IndirectY ZeroPage0

allocateZP :: Asm0 ZeroPage0
label :: Asm0 MemAddr0
lo :: MemAddr0 -> Word8
hi :: MemAddr0 -> Word8
equb :: [Word8] -> Asm0 ()
equs :: String -> Asm0 ()

and_i :: Word8 -> Asm0 ()
beq :: MemAddr0 -> Asm0 ()
bne :: MemAddr0 -> Asm0 ()
inc_m :: MemAddr0 -> Asm0 ()
iny :: Asm0 ()
jmp :: MemAddr0 -> Asm0 ()
jsr :: MemAddr0 -> Asm0 ()

class Lda arg where lda :: arg -> Asm0 ()

ldy_i :: Word8 -> Asm0 ()
lsr_a :: Asm0 ()
pha :: Asm0 ()
pla :: Asm0 ()
rts :: Asm0 ()
sta_z :: ZeroPage0 -> Asm0 ()
tax :: Asm0 ()


(>>=) = Assemble.Bind
(>>) asm1 asm2 = asm1 >>= \() -> asm2
return = Assemble.Pure
pure = Assemble.Pure
mfix = Assemble.Mfix
fail = error "UntypedAsm0.fail"

allocateZP = Assemble.AllocateZP >>= \b -> pure (ZeroPage b)
label = Assemble.Label >>= \a -> pure (MemAddr a)
lo (MemAddr a) = loByte a
hi (MemAddr a) = hiByte a
equb = Assemble.Emit
equs str = Assemble.Emit (map c2w str)

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
instance Lda Absolute where lda (Absolute (MemAddr a)) = op2 0xad a

ldy_i = op1 0xa0
lsr_a = op0 0x4a
pha = op0 0x48
pla = op0 0x68
rts = op0 0x60
sta_z (ZeroPage b) = op1 0x85 b
tax = op0 0xaa


branch :: Word8 -> MemAddr0 -> Asm0 ()
branch opcode (MemAddr a) =
  Assemble.Label >>= \here -> op1 opcode (fromIntegral (a - here - 2))

op0 :: Word8 -> Asm0 ()
op0 code = Assemble.Emit [code]

op1 :: Word8 -> Word8 -> Asm0 ()
op1 code b = Assemble.Emit [code, b]

op2 :: Word8 -> Word16 -> Asm0 ()
op2 code a = Assemble.Emit [code, loByte a, hiByte a]

loByte,hiByte :: Word16 -> Word8
loByte a = fromIntegral (a .&. 0xff)
hiByte a = fromIntegral (a `shiftR` 8)
