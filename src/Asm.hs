
module Asm
  ( Asm, VAL(..), STACK(..), CPU(..), State
  , assemble
  , (>>=), (>>), return, pure, mfix, fail
  , ZpAddr, MemAddr

  , Immediate, immediate, immChar

  , allocateZP

  , labelCode
  , labelEntry
  , labelData

  , lo, hi, equb, equs
  , ZeroPage(..), Absolute(..), IndexedX(..), IndexedY(..), IndirectY(..)

  , and_i
  , beq
  , bne
  , inc_m
  , inc_z
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

import Prelude hiding ((>>=),(>>),return,pure,fail)

import Assemble (Asm(..),assemble)
import Data.Bits (shiftR,(.&.))
import Data.ByteString.Internal (c2w)
import Data.Word (Word8,Word16)
import Effect

newtype MemAddr (g :: VAL) = MA Word16 deriving (Num)
newtype ZpAddr (v :: VAL) = ZP Word8 deriving (Num)

newtype Immediate (v :: VAL) = Immediate Word8 deriving (Num)
newtype ZeroPage v = ZeroPage (ZpAddr v)
newtype Absolute g = Absolute (MemAddr g)
newtype IndexedX g = IndexedX (MemAddr g)
newtype IndexedY g = IndexedY (MemAddr g)
newtype IndirectY v = IndirectY (ZpAddr v)


immediate :: Word8 -> Immediate ('Data Word8)
immediate = Immediate

immChar :: Char -> Immediate ('Data Char)
immChar c = Immediate (c2w c)


(>>=)
  :: Asm g1 g2 v1
  -> (v1 -> Asm g2 g3 v2)
  -> Asm g1 g3 v2

(>>)
  :: Asm g1 g2 ()
  -> Asm g2 g3 v2
  -> Asm g1 g3 v2

return :: v -> Asm g g v
pure :: v -> Asm g g v
mfix :: (v -> Asm g1 g2 v) -> Asm g1 g2 v
fail :: Asm g1 g2 v

allocateZP :: forall v g. Asm g g (ZpAddr v)

labelPermissive :: Asm v_ignore v (MemAddr v) -- not exposed to user
labelEntry :: Asm ('Data v) ('Code c) (MemAddr ('Code c)) -- entry code (no fallthrough)
labelCode :: Asm ('Code c) ('Code c) (MemAddr ('Code c)) -- expects fallthrough
labelData :: Asm ('Data v) ('Data v) (MemAddr ('Data v))

labelEntry = labelPermissive
labelCode = labelPermissive
labelData = labelPermissive

equb :: [Word8] -> Asm v v ()
equs :: String -> Asm v v ()

and_i :: Immediate a -> Asm (State a x y s) (State a x y s) ()
beq :: MemAddr ('Code c) -> Asm ('Code c) ('Code c) ()
bne :: MemAddr ('Code c) -> Asm ('Code c) ('Code c) ()
inc_m :: MemAddr ('Data v) -> Asm g g () -- no
inc_z :: ZpAddr v -> Asm g g () -- no
iny :: Asm g g ()

jmp :: MemAddr ('Code c) -> Asm ('Code c) ('Data v) ()

jsr :: MemAddr (State a1 x1 y1 ('Cons ('ReturnAddr ('Cpu a2 x2 y2 s2)) s1))
    -> Asm (State a1 x1 y1 s1)
           (State a2 x2 y2 s2) ()

ldy_i :: Immediate a -> Asm (State a x o s) (State a x y s) ()

lsr_a :: Asm g g ()

pha :: Asm (State a x y s)
           (State a x y ('Cons a s)) ()

pla :: Asm (State o x y ('Cons a s))
           (State a x y s) ()

rts :: Asm (State a x y ('Cons ('ReturnAddr ('Cpu a x y s)) s))
           ('Data v) ()

sta_z :: ZpAddr (a :: VAL) -> Asm (State a x y s) (State a x y s) ()

tax :: Asm (State a x y s) (State a a y s) ()

--class Lda arg where
--  lda :: arg -> Asm (State o x y s) (State a x y s) ()

class Lda mode where
  lda :: mode a -> Asm (State o x y s) (State a x y s) ()

lo :: MemAddr g -> Immediate a -- TODO; erm?
hi :: MemAddr g -> Immediate a

(>>=) = Bind
(>>) asm1 asm2 = asm1 >>= \() -> asm2

return = Pure
pure = Pure
mfix = Mfix
fail = error "WrappedAsm.fail"

allocateZP = AllocateZP >>= \b -> pure (ZP b)
labelPermissive = Label >>= \a -> pure (MA a)

lo (MA a) = Immediate (loByte a)
hi (MA a) = Immediate (hiByte a)

equb bs = Emit bs
equs str = Emit (map c2w str)

and_i (Immediate b) = op1 0x29 b
beq = branch 0xf0
bne = branch 0xd0
inc_m (MA a) = op2 0xee a
inc_z (ZP b) = op1 0xe6 b
iny = op0 0xc8
jmp (MA a) = op2 0x4c a
jsr (MA a) = op2 0x20 a
ldy_i (Immediate b) = op1 0xa0 b
lsr_a = op0 0x4a
pha = op0 0x48
pla = op0 0x68
rts = op0 0x60
sta_z (ZP b) = op1 0x85 b
tax = op0 0xaa

instance Lda Immediate where lda (Immediate b) = op1 0xa9 b
instance Lda IndirectY where lda (IndirectY (ZP b)) = op1 0xb1 b
instance Lda IndexedY where lda (IndexedY (MA a)) = op2 0xb9 a
instance Lda IndexedX where lda (IndexedX (MA a)) = op2 0xbd a
instance Lda Absolute where lda (Absolute (MA a)) = op2 0xad a
instance Lda ZeroPage where lda (ZeroPage (ZP b)) = op1 0xa5 b

branch :: Word8 -> MemAddr ('Code c) -> Asm ('Code c) ('Code c) ()
branch opcode (MA a) =
  Label >>= \here -> op1 opcode (fromIntegral (a - here - 2))

op0 :: Word8 -> Asm g g2 ()
op0 code = Emit [code]

op1 :: Word8 -> Word8 -> Asm g g2 ()
op1 code b = Emit [code, b]

op2 :: Word8 -> Word16 -> Asm g g2 ()
op2 code a = Emit [code, loByte a, hiByte a]

loByte,hiByte :: Word16 -> Word8
loByte a = fromIntegral (a .&. 0xff)
hiByte a = fromIntegral (a `shiftR` 8)
