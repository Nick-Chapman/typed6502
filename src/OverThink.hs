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
tay =   op0 (ByteOfWord 0xff :: TAY) -- TODO
txa =   op0 (ByteOfWord 0xff :: TXA) -- TODO

type LDA_i = forall a x y o. Byte ('Code ('Cpu o x y) ('Op1 a) ('Cpu a x y))
type LDA_z = forall a x y o. Byte ('Code ('Cpu o x y) ('Op1 ('ZpAddr a)) ('Cpu a x y))
type TAX   = forall a x y.   Byte ('Code ('Cpu a x y) ('Op0) ('Cpu a a y))
type TAY   = forall a x y.   Byte ('Code ('Cpu a x y) ('Op0) ('Cpu a x a))
type TXA   = forall a x y.   Byte ('Code ('Cpu a x y) ('Op0) ('Cpu x x y))

--[immediates] ----------------------------------------------------------------

immChar :: Char -> Byte ('Data Char)
immChar c = ByteOfWord (c2w c)

immWord :: Word8 -> Byte ('Data Word8)
immWord w = ByteOfWord w

--[interface]-------------------------------------------------------------

pure :: v -> Asm g g v
return :: v -> Asm g g v
(>>) :: Asm f g () -> Asm g h w -> Asm f h w
(>>=) :: Asm f g v -> (v -> Asm g h w) -> Asm f h w

op0
  :: Byte ('Code c 'Op0 d)
  -> Asm ('Gen z ('Seq ('Code c 'Op0 d) ('Seq ('Code d op e) m)))
         ('Gen z ('Seq                        ('Code d op e) m))
         ()

op1
  :: Byte ('Code c ('Op1 a) d)
  -> Byte a
  -> Asm ('Gen z ('Seq ('Code c ('Op1 a) d) ('Seq a ('Seq ('Code d op e) m))))
         ('Gen z ('Seq                                    ('Code d op e) m))
         ()


pure = return
return = Pure
(>>) a1 a2 = Bind a1 (\() -> a2)
(>>=) = Bind
op0 code = Emit code
op1 code b = do Emit code; Emit b

data Interpretation
  = ZpAddr Interpretation
  | Code CpuState Op CpuState
  | Data Type

data Op = Op0 | Op1 { _arg :: Interpretation }

data Byte (i::Interpretation) = ByteOfWord { _w :: Word8 }
--data MemAddr (i::Interpretation) = MemAddrOfBytePair { _lo :: Word8, _hi :: Word8  }

data Asm :: Generation -> Generation -> Type -> Type where
  Pure :: a -> Asm g g a
  Bind :: Asm f g a -> (a -> Asm g h b) -> Asm f h b
  AllocZP :: Asm ('Gen ('Seq i zp) mem) ('Gen zp mem) (Byte ('ZpAddr i))
  --Label :: Asm g h (MemAddr i)
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

