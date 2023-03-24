
-- Typed 6502 assembly

module TypedAsm
  ( Asm, assemble, Bytes
  , VAL(..), STACK(..), CPU(..), GENERATED(..), State
  , pure, (>>=), (>>), return, mfix, fail
  , halt
  , nop, inc
  , MemAddr, labelData, equb, ldaM, staM
  , JumpDest, labelCode, jmp, beq
  , ZeroPage, allocateZP, ldaZ, staZ
  , Immediate, immediate, ldaI
  , tax, txa, tay, tya
  , pha, pla
  , Flags, php, plp
  , jsr, rts
  -- TODO: bne, bcc, bcs...
  -- TODO: ldx, ldy, stx sty
  -- TODO: inx, iny, adc, asl...
  -- TODO: addressing mode: M,x  M,y  (Z,x)  (Z),y
  ) where

import Prelude hiding (pure,(>>=),(>>),return,fail)
import Data.Kind (Type)
import Data.Word (Word8)

data Bytes

assemble :: Asm ('Code c) 'NotExecutable () -> Bytes

data VAL = Value Type | ReturnAddr CPU
data STACK = Cons { _head :: VAL, _tail :: STACK }
data CPU = Cpu { _acc :: VAL, _xreg :: VAL, _yreg :: VAL, _stack :: STACK }
data GENERATED = NotExecutable | Code { _cpu :: CPU }

data Asm ( _pre :: GENERATED) ( _post :: GENERATED) v

data MemAddr (v :: VAL)
data JumpDest (s :: CPU)
data ZeroPage (v :: VAL)
data Immediate (v :: VAL)
data Flags

pure :: v -> Asm g g v
return :: v -> Asm g g v

mfix :: (v -> Asm g1 g2 v) -> Asm g1 g2 v
fail :: Asm g1 g2 v

(>>=)
  :: Asm g1 g2 v1
  -> (v1 -> Asm g2 g3 v2)
  -> Asm g1 g3 v2

(>>)
  :: Asm g1 g2 ()
  -> Asm g2 g3 v2
  -> Asm g1 g3 v2

halt :: Asm ('Code c) 'NotExecutable ()

immediate :: Word8 -> Immediate v
allocateZP :: forall v p. Asm p p (ZeroPage v)

labelData :: Asm 'NotExecutable 'NotExecutable (MemAddr v)
labelCode :: Asm g ('Code c) (JumpDest c)

equb :: Word8 -> Asm 'NotExecutable 'NotExecutable ()

jmp :: JumpDest c -> Asm ('Code c) 'NotExecutable ()
beq :: JumpDest c -> Asm ('Code c) ('Code c) ()

nop :: Asm ('Code c) ('Code c) ()
inc :: Asm ('Code c) ('Code c) ()


type State (a::VAL) (x::VAL) (y::VAL) (s::STACK) = 'Code ('Cpu a x y s)

ldaI :: Immediate a         -> Asm (State o x y s) (State a x y s) ()
ldaZ :: ZeroPage (a :: VAL) -> Asm (State o x y s) (State a x y s) ()
ldaM :: MemAddr  (a :: VAL) -> Asm (State a x y s) (State a x y s) ()

staZ :: ZeroPage (a :: VAL) -> Asm (State a x y s) (State a x y s) ()
staM :: MemAddr  (a :: VAL) -> Asm (State a x y s) (State a x y s) ()

tax :: Asm (State a x y s) (State a a y s) ()
tay :: Asm (State a x y s) (State a x a s) ()
txa :: Asm (State a x y s) (State x x y s) ()
tya :: Asm (State a x y s) (State y x y s) ()

pha :: Asm (State a x y s)
           (State a x y ('Cons a s)) ()

pla :: Asm (State o x y ('Cons a s))
           (State a x y s) ()

php :: Asm (State a x y s)
           (State a x y ('Cons ('Value Flags) s)) ()

plp :: Asm (State a x y ('Cons ('Value Flags) s))
           (State a x y s) ()

jsr :: JumpDest ('Cpu a1 x1 y1 ('Cons ('ReturnAddr ('Cpu a2 x2 y2 s2)) s1))
    -> Asm (State a1 x1 y1 s1)
           (State a2 x2 y2 s2) ()

rts :: Asm (State a x y ('Cons ('ReturnAddr ('Cpu a x y s)) s))
           'NotExecutable ()


return = pure
(>>) asm1 asm2 = asm1 >>= \() -> asm2

(   assemble
  , pure, (>>=), mfix, fail
  , halt, nop, inc
  , immediate
  , allocateZP
  , labelData
  , labelCode
  , equb
  , jmp, beq
  , ldaI, ldaZ, ldaM, staZ, staM
  , tax, txa, tay, tya
  , pha, pla
  , php, plp
  , jsr, rts
  )
  = undefined
