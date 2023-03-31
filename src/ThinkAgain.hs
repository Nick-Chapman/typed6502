{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# OPTIONS -Wno-missing-signatures #-}

module ThinkAgain (main) where

import Prelude hiding (return,pure,(>>=),(>>))
import Data.Kind (Type)
import Data.Word (Word8)
import Data.ByteString.Internal (c2w)


--main :: Asm (Data Char) (Data t) (Byte ('ZpAddr a))
main = do
  v <- allocZP
  v2 <- allocZP

  d1 <- label
  equb (immediate 'x')
  d2 <- label
  equb (immediate @Word8 0x42)

  entry
  c1 <- label
  lda_i (immediate @Word8 1)
  lda_i (immediate '1')
  c2 <- label
  lda_z v
  lda_z v2
  lda_a d1 -- (TypeError, c1)
  lda_a d2 -- (TypeError, c1)
  jmp c1 -- (TypeError, d1)

  entry
  jmp c2
  return (d1,d2)


_xx = do
  lda_i (immediate 'x')


--[interface]-------------------------------------------------------------

pure :: v -> Asm g g v
return :: v -> Asm g g v
(>>) :: Asm f g () -> Asm g h w -> Asm f h w
(>>=) :: Asm f g v -> (v -> Asm g h w) -> Asm f h w
allocZP :: Asm g g (Byte ('ZpAddr i))

label :: Asm ('Gen i g) ('Gen i g) (MemAddr i)
entry :: Asm ('Gen i g) ('Gen ('ICode c cd) g) ()

equb :: Immediate ('IData t) -> MkAsm2 ('IData t) ('IData t2) g ()
jmp :: ACode c d -> MkAsm2 ('ICode c d) ('IData t) g ()

class Imm t where immediate :: t -> Immediate ('IData t)

instance Imm Word8 where immediate = immediate_word8
instance Imm Char where immediate = immediate_char

immediate_word8 :: Word8 -> Immediate ('IData Word8)
immediate_char :: Char -> Immediate ('IData Char)

--class Lda arg a | arg -> a where lda :: arg -> MkAsm2 (Cpu o x) (Cpu a x) g ()
--class Lda arg a | arg -> a where
--  lda :: arg -> MkAsm2 (Cpu o x) (Cpu a x) g ()
  --lda :: arg -> Asm ('Gen (Cpu o x) g) ('Gen (Cpu a x) h) ()

--instance Lda (Immediate i) i where lda = lda_i
--instance Lda (Byte ('ZpAddr i)) i where lda = lda_z
--instance Lda (MemAddr ('IData t)) ('IData t) where lda = lda_a


--lda_i :: Immediate a -> MkAsm2 (Cpu o x) (Cpu a x) g ()

--lda_i :: Immediate a -> Asm ('Gen ('ICode c d) ('Gen a ('Gen k g))) ('Gen k g) ()


lda_z :: Byte ('ZpAddr a) -> MkAsm2 (Cpu o x d) (Cpu a x d) g  ()
lda_a :: MemAddr a -> MkAsm2 (Cpu o x d) (Cpu a x d) g ()



type Cpu a x d = 'ICode ('CpuComponents a x) d
type ACode c d = MemAddr ('ICode c d)

type MkAsm2 (i::ByteInterpretation) (j::ByteInterpretation) g v =
  Asm ('Gen i ('Gen j g)) ('Gen j g) v

{-type MkAsm22 (i::ByteInterpretation) (j::ByteInterpretation) g h v =
  --Asm ('Gen i ('Gen j g)) ('Gen j g) v
  Asm ('Gen i g) ('Gen j h) v-}


--[imp]--------------------------------------------------------------------

immediate_word8 w = Imm (MkByte w)
immediate_char c = Imm (MkByte (c2w c))


pure = Pure
return = pure
(>>) a1 a2 = Bind a1 (\() -> a2)
(>>=) = Bind
allocZP = AllocZP
label = Label
entry = Emit []

--lda_i (Imm b) = op1 0xa9 b
--lda_i (Imm b) = op1c (MkByte 0xa9) b
lda_i (Imm b) = op1c lda_i_op b
lda_z = op1 0xa5
lda_a = op2 0xad


lda_i_op :: Byte ('ICode ('CpuComponents o x) ('CpuComponents a x))
lda_i_op = MkByte 0xa9

op1 :: Word8 -> Byte i -> Asm g h ()
--op1 code MkByte{w} = do emit code; emit w
op1 code b = do emit code; emitI b


op1c :: Byte ('ICode c d) -> Byte i
  -> Asm ('Gen ('ICode c d) ('Gen i ('Gen ('ICode d e) g))) ('Gen ('ICode d e) g) ()
--op1c (MkByte code) MkByte{w} = do emit code; emit w
op1c code b = do emitI code; emitI b



op2 :: Word8 -> MemAddr i -> Asm g h ()
op2 code MkMemAddr{lo,hi} = do emit code; emit lo; emit hi


equb (Imm MkByte{w}) = emit w
jmp = op2 0x4c

emit :: Word8 -> Asm g h ()
emit w = Emit [w]

emitI :: Byte i -> Asm ('Gen i g) g ()
--emitI (MkByte w) = Emit [w]
emitI b = EmitI b



data Immediate i = Imm (Byte i)

data Asm :: Generation -> Generation -> Type -> Type where
  Pure :: a -> Asm g g a
  Bind :: Asm f g a -> (a -> Asm g h b) -> Asm f h b
  AllocZP :: Asm g g (Byte ('ZpAddr i))
  Label :: Asm g h (MemAddr i)
  Emit :: [Word8] -> Asm g h ()
  EmitI :: Byte i -> Asm ('Gen i g) g ()

data Byte (i::ByteInterpretation) = MkByte { w :: Word8 }

data MemAddr (i::ByteInterpretation) = MkMemAddr { lo :: Word8, hi :: Word8  }

--kinds
data ByteInterpretation
  = IData Type
  | ZpAddr ByteInterpretation
  | ICode CpuState CpuState

data CpuState = CpuComponents { _a :: ByteInterpretation
                              , _x :: ByteInterpretation
                              }

data Generation = Gen { _first :: ByteInterpretation
                      , _rest :: Generation
                      }
