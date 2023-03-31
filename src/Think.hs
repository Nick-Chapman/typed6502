{-# LANGUAGE RebindableSyntax #-}
--{-# OPTIONS -Wno-missing-signatures #-}

module Think where -- explore typing more!

import Prelude hiding (pure,(>>),(>>=))
import Data.ByteString.Internal (c2w)

import Data.Word (Word8,Word16)
import Data.Kind (Type)

--[main]--------------------------------------------------------------------

{-main = do
  --emit (immediateWord8 0x77)
  --emit (immediateChar 'x')
  pure ()
-}

{-lda_i _b = do
  emit op_lda_i
  --emit (immediateWord8 b)
  pure ()
-}

op_lda_i :: Byte ('CodeStartsHere ('Eff ('Cpu o x y) ('Cpu a x y)))
op_lda_i = Byte 0xa9

--op1_arg ::

--emitOp0 :: Byte b -> Asm ('Gen z ('Seq b m) ('CodeStartsHere e))
--                         ('Gen z m ('CodeStartsHere e))
--                         ()
--emitOp0 = undefined

--[kinds]--------------------------------------------------------------------
data ByteInterpretation
  = NoIdea
  | Uninitialized
  | DataByte Type
  | CodeStartsHere CodeEffect
  | Op1Arg
  | ZeroPageAddress ByteInterpretation

data CodeEffect =
  Eff { pre :: CpuShape
      , post :: CpuShape
      }

data CpuShape =
  Cpu { a :: ByteInterpretation
      , x :: ByteInterpretation
      , y :: ByteInterpretation
      }

data SeqByteInterpretation =
  End | Seq { first :: ByteInterpretation , rest :: SeqByteInterpretation}

data AsmGen =
  Gen { zp  :: SeqByteInterpretation
      , mem :: SeqByteInterpretation
      , fall :: FallThrough
      }

data FallThrough = Fall CpuShape | NoFall

--[types]--------------------------------------------------------------------

-- type for Word8, parameterized by it's interpretations
newtype Byte (bi::ByteInterpretation) = Byte Word8

-- same for Word16, but these can only be memory addresses
newtype Addr (bi::ByteInterpretation) = Addr Word16


-- type
data Asm (pre::AsmGen) (post::AsmGen) (v::Type)
  = AsmConstructor


data Generated (g::AsmGen) = Generated { bytes :: [Word8], numZPvars :: Int }


--[interface] --------------------------------------------------------------------

assemble :: Asm g ('Gen 'End 'End a) () -> Generated g

immediateWord8 :: Word8 -> Byte ('DataByte Word8)
immediateChar :: Char -> Byte ('DataByte Char)

allocateZP :: Asm ('Gen ('Seq ('ZeroPageAddress b) z) m e)
                  ('Gen z m e)
                  (Byte ('ZeroPageAddress b))

--TODO: try type equality -- does not work! hget monomorphism restsriction ?!

label :: Asm ('Gen z ('Seq b m) e)
             ('Gen z ('Seq b m) e)
             (Addr b)

emit :: Byte b -> Asm ('Gen z ('Seq b m) f)
                      ('Gen z m f)
                      ()


pure :: x -> Asm g g x

(>>)
  :: Asm g1 g2 ()
  -> Asm g2 g3 v2
  -> Asm g1 g3 v2

-- need different overloaded versions?
(>>=)
  :: Asm g1 g2 v1
  -> (v1 -> Asm g2 g3 v2)
  -> Asm g1 g3 v2


--[implementtation]--------------------------------------------------------------------

immediateWord8 = Byte
immediateChar c = Byte (c2w c)

(   allocateZP
  , label
  , emit
  , pure
  , (>>)
  , (>>=)
  ) = undefined

assemble = undefined
