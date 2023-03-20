
-- Typed 6502 assembly

module Asm
  ( Asm, assemble, Generated
  , Asm0, VAL(..) , STACK(..), STATE(..), EFFECT(..), TRANS(..), FALL(..)
  , pure, (>>=), (>>), return, mfix, fail
  , halt
  , nop, inc
  , MemAddr, labelData, equb, ldaM, staM
  , JumpDest, labelCode, jmp, beq
  , ZeroPage, allocateZP, ldaZP, staZP
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

data Generated (e :: EFFECT)

data VAL = Value Type | ReturnAddr EFFECT
data STACK = Cons { _head :: VAL, _tail :: STACK } -- no NIL

data STATE = S { _acc :: VAL, _xreg :: VAL, _yreg :: VAL, _stack :: STACK }
data EFFECT = E { _pre :: STATE, _post :: STATE }
data TRANS = T { _withMe :: EFFECT, _afterMe :: EFFECT }
data FALL = Fall | Break

data Asm (enter :: FALL) (trans :: TRANS) (leave :: FALL) (v :: Type)

data MemAddr (v :: VAL)
data JumpDest (e :: EFFECT)
data ZeroPage (v :: VAL)
data Immediate (v :: VAL)
data Flags

type Asm0 e v = Asm 'Fall ('T e e) 'Fall v -- no tracked effect

assemble :: Asm 'Fall ('T e e_ignored) 'Break () -> Generated e

pure :: v -> Asm0 e v
return :: v -> Asm0 e v

(>>=)
  :: Asm enter ('T e1 e2) fallthrough v1
  -> (v1 -> Asm fallthrough ('T e2 e3) leave v2)
  -> Asm enter ('T e1 e3) leave v2

(>>)
  :: Asm enter ('T e1 e2) fallthrough ()
  -> Asm fallthrough ('T e2 e3) leave v2
  -> Asm enter ('T e1 e3) leave v2

mfix :: (v -> Asm f1 e f2 v) -> Asm f1 e f2 v

fail :: Asm b t a v

halt :: Asm 'Fall ('T ('E s s) e_ignored) 'Break ()

nop :: Asm0 e ()
inc :: Asm0 e ()

immediate :: Word8 -> Immediate v
allocateZP :: forall v e. Asm0 e (ZeroPage v)
labelData :: forall v e. Asm 'Break ('T e e) 'Break (MemAddr v)
labelCode ::             Asm allowNoDrop ('T e e) 'Fall (JumpDest e)

equb :: Word8 -> Asm 'Break t_whoKnows 'Break ()

jmp :: JumpDest e -> Asm 'Fall ('T e e_ignored) 'Break ()

beq :: JumpDest e -> Asm 'Fall ('T e e) 'Break ()


ldaI :: Immediate a
     -> Asm 'Fall ('T ('E ('S o x y s) end)
                      ('E ('S a x y s) end)
                  ) 'Fall ()

ldaZP :: ZeroPage (a :: VAL)
     -> Asm 'Fall ('T ('E ('S o x y s) end)
                      ('E ('S a x y s) end)
                  ) 'Fall ()

ldaM :: MemAddr (a :: VAL)
     -> Asm 'Fall ('T ('E ('S o x y s) end)
                      ('E ('S a x y s) end)
                  ) 'Fall ()

staZP :: ZeroPage (a :: VAL)
     -> Asm 'Fall ('T ('E ('S a x y s) end)
                      ('E ('S a x y s) end)
                  ) 'Fall ()

staM :: MemAddr (a :: VAL)
     -> Asm 'Fall ('T ('E ('S a x y s) end)
                      ('E ('S a x y s) end)
                  ) 'Fall ()


tax :: Asm 'Fall ('T ('E ('S a x y s) end)
                     ('E ('S a a y s) end)
                 ) 'Fall ()

txa :: Asm 'Fall ('T ('E ('S a x y s) end)
                     ('E ('S x x y s) end)
                 ) 'Fall ()

tay :: Asm 'Fall ('T ('E ('S a x y s) end)
                     ('E ('S a x a s) end)
                 ) 'Fall ()

tya :: Asm 'Fall ('T ('E ('S a x y s) end)
                     ('E ('S y x y s) end)
                 ) 'Fall ()


pha :: Asm 'Fall ('T ('E ('S a x y          s)  end)
                     ('E ('S a x y ('Cons a s)) end)
                  ) 'Fall ()

pla :: Asm 'Fall ('T ('E ('S o x y ('Cons a s)) end)
                     ('E ('S a x y          s ) end)
                  ) 'Fall ()

php :: Asm 'Fall ('T ('E ('S a x y                       s)  end)
                     ('E ('S a x y ('Cons ('Value Flags) s)) end)
                  ) 'Fall ()

plp :: Asm 'Fall ('T ('E ('S a x y ('Cons ('Value Flags) s)) end)
                     ('E ('S a x y                       s)  end)
                  ) 'Fall ()

jsr :: JumpDest
       ('E ('S a x y ('Cons ('ReturnAddr ('E ret end)) s))
           ret)
    -> Asm 'Fall ('T ('E ('S a x y s) end)
                     ('E ret end)
                 ) 'Fall ()


rts :: Asm 'Fall ('T ('E ('S a x y
                          ('Cons ('ReturnAddr ('E ('S a x y s) end)) s)
                         ) end)
                     e_ignored
                 ) 'Break ()



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
  , ldaI, ldaZP, ldaM
  , staZP, staM
  , tax, txa, tay, tya
  , pha, pla
  , php, plp
  , jsr, rts
  )
  = undefined
