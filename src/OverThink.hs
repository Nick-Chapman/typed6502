{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS -Wno-missing-signatures #-}

module OverThink where

import Prelude hiding (return,pure,(>>=),(>>))
import Data.Kind (Type)
import Data.Word (Word8)
import Data.ByteString.Internal (c2w)

--[ops] --------------------------------------------------------------------

lda_i = op1 (ByteOfWord 0xa9 :: LDA_i)
lda_z = op1 (ByteOfWord 0xa5 :: LDA_z)
tax =   op0 (ByteOfWord 0xaa :: TAX)
-- TODO: need corrrect op codes
tay =   op0 (ByteOfWord 0xff :: TAY)
txa =   op0 (ByteOfWord 0xff :: TXA)
sta_z = op1 (ByteOfWord 0xff :: STA_z)
sta_a = op2 (ByteOfWord 0xff :: STA_a)

type LDA_i = forall a x y o.
  Byte ('Code ('Cpu o x y) ('ArgB a) ('Cpu a x y))

type LDA_z = forall a x y o zp.
  Byte ('Code ('Cpu o x y) ('ArgB ('ZpAddr ('Seq a zp))) ('Cpu a x y))

type STA_z = forall a x y zp.
  Byte ('Code ('Cpu a x y) ('ArgB ('ZpAddr ('Seq a zp))) ('Cpu a x y))

type STA_a = forall a x y is.
  Byte ('Code ('Cpu a x y) ('ArgA ('Seq a is)) ('Cpu a x y))

type TAX = forall a x y.
  Byte ('Code ('Cpu a x y) ('NoArg) ('Cpu a a y))

type TAY = forall a x y.
  Byte ('Code ('Cpu a x y) ('NoArg) ('Cpu a x a))

type TXA = forall a x y.
  Byte ('Code ('Cpu a x y) ('NoArg) ('Cpu x x y))

--[immediates] ----------------------------------------------------------------

immChar :: Char -> Byte ('Data Char)
immChar c = ByteOfWord (c2w c)

immWord :: Word8 -> Byte ('Data Word8)
immWord w = ByteOfWord w

nextZ :: Byte ('ZpAddr ('Seq i is)) -> Byte ('ZpAddr is)
nextZ ByteOfWord{w} = ByteOfWord (w + 1)

--[interface]-------------------------------------------------------------

pure :: v -> Asm g g v
return :: v -> Asm g g v
(>>) :: Asm f g () -> Asm g h w -> Asm f h w
(>>=) :: Asm f g v -> (v -> Asm g h w) -> Asm f h w

op0
  :: Byte ('Code c 'NoArg d)
  -> Asm ('Gen z ('Seq ('Code c 'NoArg d) ('Seq ('Code d op e) m)))
         ('Gen z ('Seq                        ('Code d op e) m))
         ()

op1
  :: Byte ('Code c ('ArgB a) d)
  -> Byte a
  -> Asm ('Gen z ('Seq ('Code c ('ArgB a) d) ('Seq a ('Seq ('Code d op e) m))))
         ('Gen z ('Seq                                    ('Code d op e) m))
         ()

op2
  :: Byte ('Code c ('ArgA is) d)
  -> MemAddr is
  -> Asm ('Gen z ('Seq ('Code c ('ArgA is) d)
                  ('Seq ('LoByteOfAddr is)
                   ('Seq ('HiByteOfAddr is)
                    ('Seq ('Code d op e) m)))))
         ('Gen z
                    ('Seq ('Code d op e) m))
         ()

--[imp]-------------------------------------------------------------

pure = return
return = Pure
(>>) a1 a2 = Bind a1 (\() -> a2)
(>>=) = Bind
op0 code = Emit code
op1 code b = do Emit code; Emit b
op2 code MemAddrOfBytePair{lo,hi} = do Emit code; Emit lo; Emit hi

data Interpretation
  = ZpAddr SeqInterpretation
  | Code CpuState Op CpuState
  | Data Type
  | LoByteOfAddr SeqInterpretation
  | HiByteOfAddr SeqInterpretation

data Op
  = NoArg
  | ArgB { _byte :: Interpretation }
  | ArgA { _addr :: SeqInterpretation }

data Byte (i::Interpretation) = ByteOfWord { w :: Word8 }

data MemAddr (i::SeqInterpretation) =
  MemAddrOfBytePair { lo :: Byte ('LoByteOfAddr i)
                    , hi :: Byte ('HiByteOfAddr i)
                    }

data Asm :: Generation -> Generation -> Type -> Type where
  Pure :: a -> Asm g g a
  Bind :: Asm f g a -> (a -> Asm g h b) -> Asm f h b
  AllocZP :: Asm ('Gen ('Seq i zp) mem) ('Gen zp mem) (Byte ('ZpAddr ('Seq i zp)))
  Label :: Asm ('Gen zp mem) ('Gen zp mem) (MemAddr mem)
  Emit :: Byte i -> Asm ('Gen zp ('Seq i mem)) ('Gen zp mem) ()

data CpuState = Cpu { _a :: Interpretation
                    , _x :: Interpretation
                    , _y :: Interpretation
                    }

data Generation = Gen { _zp :: SeqInterpretation
                      , _mem :: SeqInterpretation
                      }

data SeqInterpretation = Seq { _first :: Interpretation
                             , _rest :: SeqInterpretation
                             }
