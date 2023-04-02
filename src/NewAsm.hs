{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# OPTIONS -Wno-unticked-promoted-constructors #-}
{-# OPTIONS -Wno-missing-signatures #-}

module NewAsm where

import Data.Kind (Type)
import Data.Word (Word8)
import Prelude hiding (return,pure,(>>=),(>>))
import Data.ByteString.Internal (c2w)

main = do
  rts ; rts -- ok
  --nop ; nop -- Totally bust  :(

----------------------------------------------------------------------

equb = Emit

----------------------------------------------------------------------

class DataByte t where byte :: t -> Byte ('Data t)

instance DataByte Char where byte = byteChar
instance DataByte Word8 where byte = byteWord

byteChar :: Char -> Byte (Data Char)
byteChar c = Byte (c2w c)

byteWord :: Word8 -> Byte (Data Word8)
byteWord = Byte

nextZ :: Byte (ZpAddr (Seq i is)) -> Byte (ZpAddr is)
nextZ Byte{w} = Byte (w + 1)

----------------------------------------------------------------------

nop = (op0 :: Implied NOP) (Byte undefined)
pha = (op0 :: Implied PHA) (Byte 0x48)
pla = (op0 :: Implied PLA) (Byte 0x68)
tax = (op0 :: Implied TAX) (Byte 0xaa)
txa = (op0 :: Implied TXA) (Byte undefined)

lda_i = (opB :: Immediate a (LDA a)) (Byte 0xa9)
lda_z = (opB :: ZeroPage  a (LDA a)) (Byte 0xa5)
lda_m = (opA :: Absolute  a (LDA a)) (Byte 0xad)

sta_z = (opB :: ZeroPage  a (STA a)) (Byte 0x85)
sta_m = (opA :: Absolute  a (STA a)) (Byte 0x8d)

jmp = (opA :: JMP) (Byte 0x4c)
jsr = (opA :: JSR) (Byte undefined)
rts = (op0 :: RTS) (Byte 0x60)


----------------------------------------------------------------------

type LDA :: Interpretation -> Effect
type STA :: Interpretation -> Effect
type TAX :: Effect
type TXA :: Effect
type PHA :: Effect
type PLA :: Effect
type NOP :: Effect

type JMP :: Type
type RTS :: Type
type JSR :: Type

type LDA a = forall a x y s o. '( Cpu o x y s, Cpu a x y s)
type STA a = forall a x y s.   '( Cpu a x y s, Cpu a x y s)

type NOP = forall a x y s. '( Cpu a x y s, Cpu a x y s)
type PHA = forall a x y s. '( Cpu a x y s, Cpu a x y (UserData a s))
type PLA = forall a x y s. '( Cpu a x y (UserData a s), Cpu a x y s)
type TAX = forall a x y s. '( Cpu a x y s, Cpu a a y s)
type TXA = forall a x y s. '( Cpu a x y s, Cpu x x y s)

type JMP = forall c. TransferA c
type RTS = forall a x y s. Transfer0 (Cpu a x y (RetAddr (Cpu a x y s) s))

type JSR = forall a1 x1 y1 a2 x2 y2 s.
  OpA (Cpu a1 x1 y1 s) (Cpu a2 x2 y2 s)
  (Code (Cpu a1 x1 y1 (RetAddr (Cpu a2 x2 y2 s) s)))


----------------------------------------------------------------------

type Implied :: Mode
type Immediate :: Mode1
type Absolute :: Mode1
type ZeroPage :: Mode1

type Mode1 = Interpretation -> Mode
type Mode = Effect -> Type

type Effect = (CpuState,CpuState)

----------------------------------------------------------------------

type Implied eff = Op0 (Pre eff) (Post eff)

type ZeroPage i eff = forall is. Immediate (ZpAddr (Seq i is)) eff
type Immediate i eff = OpB (Pre eff) (Post eff) i
type Absolute i eff = OpA (Pre eff) (Post eff) i

type Pre :: Effect -> CpuState
type family Pre e where
  Pre '(c,d) = c

type Post :: Effect -> CpuState
type family Post e where
  Post '(c,d) = c

----------------------------------------------------------------------

type Op0 :: CpuState -> CpuState -> Type
type Op0 c d = forall z m.
  Byte (Code c) ->
  Asm (Gen z (Seq (Code c) (Seq (Code d) m)))
      (Gen z               (Seq (Code d) m))
      ()

type OpB :: CpuState -> CpuState -> Interpretation -> Type
type OpB c d i = forall z m.
  Byte (Code c) ->
  Byte i ->
  Asm (Gen z (Seq (Code c) (Seq i (Seq (Code d) m))))
      (Gen z                      (Seq (Code d) m))
      ()

type OpA :: CpuState -> CpuState -> Interpretation -> Type
type OpA c d i = forall z m is.
  Byte (Code c) ->
  Addr (Seq i is) ->
  Asm (Gen z (Seq (Code c)
               (Seq (LoByteOfAddr (Seq i is))
                (Seq (HiByteOfAddr (Seq i is))  (Seq (Code d) m)))))
      (Gen z                                    (Seq (Code d) m))
      ()


-- for instructions which transfer control with no return (jmp,rts)
-- the type is simpler because there is no fallthrough

type Transfer0 :: CpuState -> Type
type Transfer0 c = forall z m.
  Byte (Code c) ->
  Asm (Gen z (Seq (Code c) m))
      (Gen z               m)
      ()

type TransferA :: CpuState -> Type
type TransferA c = forall z m is.
  Byte (Code c) ->
  Addr (Seq (Code c) is) ->
  Asm (Gen z (Seq (Code c)
               (Seq (LoByteOfAddr (Seq (Code c) is))
                (Seq (HiByteOfAddr (Seq (Code c) is))  m))))
      (Gen z                                           m)
      ()


----------------------------------------------------------------------

pure = return
return = Pure
(>>) a1 a2 = Bind a1 (\() -> a2)
(>>=) = Bind
op0 = Emit
opB code b = do Emit code; Emit b
opA code Addr{lo,hi} = do Emit code; Emit lo; Emit hi

----------------------------------------------------------------------

data Byte (i::Interpretation) = Byte { w :: Word8 }

data Addr (i::SeqInterpretation) =
  Addr { lo :: Byte (LoByteOfAddr i)
       , hi :: Byte (HiByteOfAddr i)
       }

data Asm :: Generation -> Generation -> Type -> Type where
  Pure :: a -> Asm g g a
  Bind :: Asm f g a -> (a -> Asm g h b) -> Asm f h b
  AllocZP :: Asm (Gen (Seq i z) m) (Gen z m) (Byte (ZpAddr (Seq i z)))
  Label :: Asm (Gen z m) (Gen z m) (Addr m)
  Emit :: Byte i -> Asm (Gen z (Seq i m)) (Gen z m) ()


----------------------------------------------------------------------

data Generation = Gen { _zp :: SeqInterpretation
                      , _mem :: SeqInterpretation
                      }

data CpuState =
  Cpu { _a :: Interpretation
      , _x :: Interpretation
      , _y :: Interpretation
      , _s :: Stack
      }

data Stack = RetAddr CpuState Stack
           | UserData Interpretation Stack

data Interpretation
  = ZpAddr SeqInterpretation
  | Code CpuState
  | Data Type
  | LoByteOfAddr SeqInterpretation
  | HiByteOfAddr SeqInterpretation

data SeqInterpretation = Seq { _first :: Interpretation
                              , _rest :: SeqInterpretation
                              }
