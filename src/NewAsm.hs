{-# OPTIONS -Wno-unticked-promoted-constructors #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE RebindableSyntax #-}

{-# OPTIONS -Wno-missing-signatures #-}

module NewAsm where

import Data.Kind (Type)
import Data.Word (Word8)
import Prelude hiding (return,pure,(>>=),(>>))
import Data.ByteString.Internal (c2w)

--main :: Final (Cpu (Data Word8) (Data Char) y s) ()
main = final $ do
  v1 <- AllocZP
  v2 <- AllocZP
  lda_i (byte 'x')
  sta_z v1
  tax
  lda_i (byte @Word8 12)
  sta_z v2
  pure (v1,v2)

final :: Asm g (Gen z (Seq (Code cpu) h)) v -> Final cpu v
final = undefined
data Final (cpu :: CpuState) v

----------------------------------------------------------------------

lda_m = (opA :: LDA_m) (Byte 0xad)
sta_m = (opA :: STA_m) (Byte 0x8d)
--lda_i = (opB :: LDA_i) (Byte 0xa9)
lda_i = (opB :: OpB (Cpu o x y s) (Cpu a x y s) a) (Byte 0xa9) -- example inlined def
sta_z = (opB :: STA_z) (Byte 0x85)
lda_z = (opB :: LDA_z) (Byte 0xa9)

nop = (op0 :: NOP) (Byte undefined)
pha = (op0 :: PHA) (Byte 0x48)
pla = (op0 :: PLA) (Byte 0x68)
tax = (op0 :: TAX) (Byte 0xaa)
txa = (op0 :: TXA) (Byte undefined)

jmp = (opA :: JMP) (Byte 0x4c)
jsr = (opA :: JSR) (Byte undefined)
rts = (op0 :: RTS) (Byte 0x60)

----------------------------------------------------------------------

-- These could be inlined, saving writing the forall...

type NOP = forall a x y s. Op0 (Cpu a x y s) (Cpu a x y s)
type PHA = forall a x y s. Op0 (Cpu a x y s) (Cpu a x y (UserData a s))
type PLA = forall a x y s. Op0 (Cpu a x y (UserData a s)) (Cpu a x y s)
type TAX = forall a x y s. Op0 (Cpu a x y s) (Cpu a a y s)
type TXA = forall a x y s. Op0 (Cpu a x y s) (Cpu x x y s)

type LDA_i = forall a x y s o. OpB (Cpu o x y s) (Cpu a x y s) a
type LDA_z = forall a x y s o. OpZ (Cpu o x y s) (Cpu a x y s) a
type LDA_m = forall a x y s o. OpA (Cpu o x y s) (Cpu a x y s) a

type STA_z = forall a x y s.   OpZ (Cpu a x y s) (Cpu a x y s) a
type STA_m = forall a x y s.   OpA (Cpu a x y s) (Cpu a x y s) a

type JMP = forall c. TransferA c
type RTS = forall a x y s. Transfer0 (Cpu a x y (RetAddr (Cpu a x y s) s))

type JSR = forall a1 x1 y1 a2 x2 y2 s.
  OpA (Cpu a1 x1 y1 s) (Cpu a2 x2 y2 s)
  (Code (Cpu a1 x1 y1 (RetAddr (Cpu a2 x2 y2 s) s)))

----------------------------------------------------------------------

type OpZ :: CpuState -> CpuState -> Interpretation -> Type
type OpZ c d i = forall is.
  OpB c d (ZpAddr (Seq i is))


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

pure = return
return = Pure
(>>) a1 a2 = Bind a1 (\() -> a2)
(>>=) = Bind
op0 = Emit
opB code b = do Emit code; Emit b
opA code Addr{lo,hi} = do Emit code; Emit lo; Emit hi
equb = Emit

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
