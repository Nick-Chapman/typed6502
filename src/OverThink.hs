{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS -Wno-missing-signatures #-}

module OverThink where

import Prelude hiding (return,pure,(>>=),(>>))
import Data.Kind (Type)
import Data.Word (Word8)
import Data.ByteString.Internal (c2w)

--[ops] --------------------------------------------------------------------

-- TODO: need corrrect op codes

--jsr (arg::MemAddr ('Code c d)) =
--  op2tx (ByteOfWord 0x4c :: JSR c d) arg


--jsr (arg::MemAddr a) =
--  op2tx (ByteOfWord 0x20 :: JSR) arg

--type JSR = forall a x y s c d.
--  Byte ('Code ('Cpu a x y s) ('Cpu a x y ('RetAddr c d s)))



rts = op0tx (ByteOfWord 0x60 :: RTS)

type RTS = forall a x y c d s.
  Byte ('Code ('Cpu a x y ('RetAddr c d s)) ('Cpu a x y s))


jmp (arg::MemAddr ('Code c d)) =
  op2tx (ByteOfWord 0x4c :: Byte ('Code c d)) arg

lda_i (arg::Byte a)                     = op1 (ByteOfWord 0xa9 :: LDA a) arg
lda_z (arg::Byte ('ZpAddr ('Seq a zp))) = op1 (ByteOfWord 0xa5 :: LDA a) arg
sta_a (arg::MemAddr a)                  = op2 (ByteOfWord 0xff :: STA a) arg
sta_z (arg::Byte ('ZpAddr ('Seq a zp))) = op1 (ByteOfWord 0xff :: STA a) arg
tax                                     = op0 (ByteOfWord 0xaa :: TAX)
tay                                     = op0 (ByteOfWord 0xff :: TAY)
txa                                     = op0 (ByteOfWord 0xff :: TXA)

type LDA a = forall x y s o.
  Byte ('Code ('Cpu o x y s) ('Cpu a x y s))

type STA a = forall x y s.
  Byte ('Code ('Cpu a x y s) ('Cpu a x y s))

type TAX = forall a x y s.
  Byte ('Code ('Cpu a x y s) ('Cpu a a y s))

type TAY = forall a x y s.
  Byte ('Code ('Cpu a x y s) ('Cpu a x a s))

type TXA = forall a x y s.
  Byte ('Code ('Cpu a x y s) ('Cpu x x y s))


--[bytes]----------------------------------------------------------------

class DataByte t where byte :: t -> Byte ('Data t)

instance DataByte Char where byte = byteChar
instance DataByte Word8 where byte = byteWord

byteChar :: Char -> Byte ('Data Char)
byteChar c = ByteOfWord (c2w c)

byteWord :: Word8 -> Byte ('Data Word8)
byteWord w = ByteOfWord w

nextZ :: Byte ('ZpAddr ('Seq i is)) -> Byte ('ZpAddr is)
nextZ ByteOfWord{w} = ByteOfWord (w + 1)

--[interface]-------------------------------------------------------------

pure :: v -> Asm g g v
return :: v -> Asm g g v
(>>) :: Asm f g () -> Asm g h w -> Asm f h w
(>>=) :: Asm f g v -> (v -> Asm g h w) -> Asm f h w

op0
  :: Byte ('Code c d)
  -> Asm ('Gen z ('Seq ('Code c d) ('Seq ('Code d e) m)))
         ('Gen z                   ('Seq ('Code d e) m))
         ()

op1
  :: Byte ('Code c d)
  -> Byte i
  -> Asm ('Gen z ('Seq ('Code c d) ('Seq i ('Seq ('Code d e) m))))
         ('Gen z                           ('Seq ('Code d e) m))
         ()

op2
  :: Byte ('Code c d)
  -> MemAddr i
  -> Asm ('Gen z ('Seq ('Code c d)
                  ('Seq ('LoByteOfAddr i)
                   ('Seq ('HiByteOfAddr i)
                                                ('Seq ('Code d e) m)))))
         ('Gen z                                ('Seq ('Code d e) m))
         ()

-- for instructions which transfer control (jmp,jsr,rts)
-- the type is weaker because there is no fallthrough

op0tx
  :: Byte ('Code c d)
  -> Asm ('Gen z ('Seq ('Code c d) m))
         ('Gen z                   m)
         ()

op2tx
  :: Byte ('Code c d)
  -> MemAddr i
  -> Asm ('Gen z ('Seq ('Code c d)
                  ('Seq ('LoByteOfAddr i)
                   ('Seq ('HiByteOfAddr i)      m))))
         ('Gen z                                m)
         ()

equb
  :: Byte i -> Asm ('Gen z ('Seq i m))
                   ('Gen z         m) ()

--[imp]-------------------------------------------------------------

pure = return
return = Pure
(>>) a1 a2 = Bind a1 (\() -> a2)
(>>=) = Bind
op0tx code = Emit code
op0 code = Emit code
dep_op1 code b = do Emit code; Emit b
op1 code b = do Emit code; Emit b
op2 = op2tx
op2tx code MemAddrOfBytePair{lo,hi} = do Emit code; Emit lo; Emit hi
equb b = Emit b

data Interpretation
  = ZpAddr SeqInterpretation
  | Code CpuState CpuState
  | Data Type
  | LoByteOfAddr Interpretation
  | HiByteOfAddr Interpretation

data Byte (i::Interpretation) = ByteOfWord { w :: Word8 }

data MemAddr (i::Interpretation) =
  MemAddrOfBytePair { lo :: Byte ('LoByteOfAddr i)
                    , hi :: Byte ('HiByteOfAddr i)
                    }

data Asm :: Generation -> Generation -> Type -> Type where
  Pure :: a -> Asm g g a
  Bind :: Asm f g a -> (a -> Asm g h b) -> Asm f h b
  AllocZP :: Asm ('Gen ('Seq i zp) mem) ('Gen zp mem) (Byte ('ZpAddr ('Seq i zp)))
  Label :: Asm ('Gen zp ('Seq a mem)) ('Gen zp ('Seq a mem)) (MemAddr a)
  Emit :: Byte i -> Asm ('Gen zp ('Seq i mem)) ('Gen zp mem) ()

data CpuState = Cpu { _a :: Interpretation
                    , _x :: Interpretation
                    , _y :: Interpretation
                    , _s :: Stack
                    }

data Stack
  = RetAddr CpuState CpuState Stack

data Generation = Gen { _zp :: SeqInterpretation
                      , _mem :: SeqInterpretation
                      }

data SeqInterpretation = Seq { _first :: Interpretation
                             , _rest :: SeqInterpretation
                             }
