
module Asm
  ( Asm, VAL(..), STACK(..), CPU(..), GENERATED(..), State
  , assemble
  , (>>=), (>>), return, pure, mfix, fail
  , ZpAddr, MemAddr
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

import Data.Bits (shiftR,(.&.))
import Data.ByteString.Internal (c2w)
import Data.Kind (Type)
import Data.Word (Word8,Word16)
import Prelude hiding ((>>=),(>>),return,pure,fail)
import qualified Assemble (Asm(..),assemble)

type State (a::VAL) (x::VAL) (y::VAL) (s::STACK) = 'Code ('Cpu a x y s)

newtype MemAddr (g :: GENERATED) = MA Word16 deriving (Num)
newtype ZpAddr (v :: VAL) = ZP Word8 deriving (Num)

newtype ZeroPage g = ZeroPage (ZpAddr g)
newtype Absolute g = Absolute (MemAddr g)
newtype IndexedX g = IndexedX (MemAddr g)
newtype IndexedY g = IndexedY (MemAddr g)
newtype IndirectY g = IndirectY (ZpAddr g)

data VAL = Value Type | ReturnAddr CPU
data STACK = Cons { _head :: VAL, _tail :: STACK }
data CPU = Cpu { _acc :: VAL, _xreg :: VAL, _yreg :: VAL, _stack :: STACK }
data GENERATED = NotExecutable | Code { _cpu :: CPU }

data Asm ( pre :: GENERATED) ( post :: GENERATED) v = Asm { unAsm :: Assemble.Asm v }

assemble :: Word16 -> Asm ('Code c) 'NotExecutable () -> [Word8]

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

labelPermissive :: Asm g_ignore g (MemAddr g) -- not exposed to user
labelEntry :: Asm 'NotExecutable ('Code c) (MemAddr ('Code c)) -- entry code (no fallthrough)
labelCode :: Asm ('Code c) ('Code c) (MemAddr ('Code c)) -- expects fallthrough
labelData :: Asm 'NotExecutable 'NotExecutable (MemAddr 'NotExecutable)

labelEntry = labelPermissive
labelCode = labelPermissive
labelData = labelPermissive

equb :: [Word8] -> Asm 'NotExecutable 'NotExecutable ()
equs :: String -> Asm 'NotExecutable 'NotExecutable ()

and_i :: Word8 -> Asm (State o x y s) (State a x y s) () -- TODO: need Immediate
beq :: MemAddr ('Code c) -> Asm ('Code c) ('Code c) ()
bne :: MemAddr ('Code c) -> Asm ('Code c) ('Code c) ()
inc_m :: MemAddr 'NotExecutable -> Asm g g () -- no
inc_z :: ZpAddr v -> Asm g g () -- no
iny :: Asm g g ()

jmp :: MemAddr ('Code c) -> Asm ('Code c) 'NotExecutable ()

jsr :: MemAddr (State a1 x1 y1 ('Cons ('ReturnAddr ('Cpu a2 x2 y2 s2)) s1))
    -> Asm (State a1 x1 y1 s1)
           (State a2 x2 y2 s2) ()

ldy_i :: Word8 -> Asm (State a x o s) (State a x y s) () -- TODO: immediate

lsr_a :: Asm g g ()

pha :: Asm (State a x y s)
           (State a x y ('Cons a s)) ()

pla :: Asm (State o x y ('Cons a s))
           (State a x y s) ()

rts :: Asm (State a x y ('Cons ('ReturnAddr ('Cpu a x y s)) s))
           'NotExecutable ()

sta_z :: ZpAddr (a :: VAL) -> Asm (State a x y s) (State a x y s) ()

tax :: Asm (State a x y s) (State a a y s) ()

class Lda arg where lda :: arg -> Asm (State o x y s) (State a x y s) ()

lo :: MemAddr g -> Word8 -- TODO; erm?
hi :: MemAddr g -> Word8

assemble origin (Asm m) = Assemble.assemble origin m
(>>=) (Asm m) f = Asm (Assemble.Bind m $ \v -> unAsm (f v))
(>>) asm1 asm2 = asm1 >>= \() -> asm2

return v = Asm (Assemble.Pure v)
pure = return
mfix f = Asm (Assemble.Mfix (unAsm . f))
fail = error "WrappedAsm.fail"

allocateZP = Asm Assemble.AllocateZP >>= \b -> pure (ZP b)
labelPermissive = Asm Assemble.Label >>= \a -> pure (MA a)

lo (MA a) = loByte a
hi (MA a) = hiByte a

equb bs = Asm (Assemble.Emit bs)
equs str = Asm (Assemble.Emit (map c2w str))

and_i b = op1 0x29 b
beq a = branch 0xf0 a
bne = branch 0xd0
inc_m (MA a) = op2 0xee a
inc_z (ZP b) = op1 0xe6 b
iny = op0 0xc8
jmp (MA a) = op2 0x4c a
jsr (MA a) = op2 0x20 a
ldy_i = op1 0xa0
lsr_a = op0 0x4a
pha = op0 0x48
pla = op0 0x68
rts = op0 0x60
sta_z (ZP b) = op1 0x85 b
tax = op0 0xaa

instance Lda Word8 where lda = op1 0xa9
instance Lda Char where lda c = lda (c2w c)

instance Lda (IndirectY g) where lda (IndirectY (ZP b)) = op1 0xb1 b
instance Lda (IndexedY g) where lda (IndexedY (MA a)) = op2 0xb9 a
instance Lda (IndexedX g) where lda (IndexedX (MA a)) = op2 0xbd a
instance Lda (Absolute g) where lda (Absolute (MA a)) = op2 0xad a
instance Lda (ZeroPage g) where lda (ZeroPage (ZP b)) = op1 0xa5 b

branch :: Word8 -> MemAddr g -> Asm g g2 ()
branch opcode (MA a) =
  Asm Assemble.Label >>= \here -> op1 opcode (fromIntegral (a - here - 2))

op0 :: Word8 -> Asm g g2 ()
op0 code = Asm (Assemble.Emit [code])

op1 :: Word8 -> Word8 -> Asm g g2 ()
op1 code b = Asm (Assemble.Emit [code, b])

op2 :: Word8 -> Word16 -> Asm g g2 ()
op2 code a = Asm (Assemble.Emit [code, loByte a, hiByte a])

loByte,hiByte :: Word16 -> Word8
loByte a = fromIntegral (a .&. 0xff)
hiByte a = fromIntegral (a `shiftR` 8)
