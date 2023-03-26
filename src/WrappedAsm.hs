
module WrappedAsm
  ( Asm, VAL(..), STACK(..), CPU(..), GENERATED(..), State
  , assemble
  , (>>=), (>>), return, pure, mfix, fail
  , ZpAddr, MemAddr
  , allocateZP

  , labelCode
  , labelEntry
  , labelData

  , lo, hi, equb, equs
  , Absolute(..), IndexedX(..), IndexedY(..), IndirectY(..)

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

import Data.Word (Word8,Word16)
import Phantom
import Prelude hiding ((>>=),(>>),return,pure,fail)
import qualified SimpleAsm as Simple -- TODO: avoid layering on Simple; just confusing!

newtype Absolute g = Absolute (MemAddr g)
newtype IndexedX g = IndexedX (MemAddr g)
newtype IndexedY g = IndexedY (MemAddr g)
newtype IndirectY g = IndirectY (ZpAddr g)

data Asm ( pre :: GENERATED) ( post :: GENERATED) v =
  Asm { unAsm :: Simple.Asm0 v }

newtype MemAddr (g :: GENERATED) = MA Simple.MemAddr0 deriving Num
newtype ZpAddr (v :: VAL) = ZP Simple.ZpAddr0 deriving Num

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

class Lda arg where lda :: arg -> Asm q r ()

lo :: MemAddr g -> Word8 -- TODO; erm?
hi :: MemAddr g -> Word8


assemble origin (Asm m) = Simple.assemble origin m
(>>=) (Asm m) f = Asm ((Simple.>>=) m $ \v -> unAsm (f v))
(>>) asm1 asm2 = asm1 >>= \() -> asm2

return v = Asm (Simple.return v)
pure = return
mfix f = Asm (Simple.mfix (unAsm . f))
fail = error "WrappedAsm.fail"


mapA :: (a -> b) -> Asm g1 g2 a -> Asm g1 g2 b
mapA f m = m >>= \x -> pure (f x)

allocateZP = mapA ZP (Asm Simple.allocateZP)
labelPermissive = mapA MA (Asm Simple.label)
lo (MA a) = Simple.lo a
hi (MA a) = Simple.hi a
equb = lift1 Simple.equb
equs = lift1 Simple.equs
and_i = lift1 Simple.and_i
beq (MA a) = Asm (Simple.beq a)
bne (MA a) = Asm (Simple.bne a)
inc_m (MA a) = Asm (Simple.inc_m a)
iny = Asm Simple.iny
jmp (MA a) = Asm (Simple.jmp a)
jsr (MA a) = Asm (Simple.jsr a)
ldy_i = lift1 Simple.ldy_i
lsr_a = Asm Simple.lsr_a
pha = Asm Simple.pha
pla = Asm Simple.pla
rts = Asm Simple.rts
sta_z (ZP a) = Asm (Simple.sta_z a)
tax = Asm Simple.tax

instance Lda Word8 where lda = lift1 Simple.lda
instance Lda Char where lda = lift1 Simple.lda

instance Lda (IndirectY g) where
  lda (IndirectY (ZP x)) = Asm (Simple.lda (Simple.IndirectY x))

instance Lda (IndexedY g) where
  lda (IndexedY (MA x)) = Asm (Simple.lda (Simple.IndexedY x))

instance Lda (IndexedX g) where
  lda (IndexedX (MA x)) = Asm (Simple.lda (Simple.IndexedX x))

instance Lda (Absolute g) where
  lda (Absolute (MA x)) = Asm (Simple.lda (Simple.Absolute x))

lift1 :: (a -> Simple.Asm0 v) -> a -> Asm p q v
lift1 f x = Asm (f x)
