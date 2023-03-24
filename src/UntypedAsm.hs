
module UntypedAsm
  ( Asm, assemble, writeBytes
  , pure, (>>=), (>>), return, mfix, fail
  , allocateZP, label
  , equb, equs
  , lda_i_char, lda_i, lda_my, ldy_i
  , sta_z
  , jsr, jmp
  , beq, bne
  , iny
  , lo, hi
  ) where

import Prelude hiding (pure,(>>=),(>>),return,fail)

import Data.Bits (shiftR,(.&.))
import Data.ByteString.Internal (c2w)
import Data.Word (Word8,Word16)
import Text.Printf (printf)
import qualified Data.ByteString as ByteString (pack,writeFile)

writeBytes :: FilePath -> [Word8] -> IO ()
writeBytes path bs = do
  printf "writeBytes (#%d) --> %s\n" (length bs) path
  ByteString.writeFile path (ByteString.pack bs)


-- interface

return :: v -> Asm v
pure :: v -> Asm v
(>>=) :: Asm v1 -> (v1 -> Asm v2) -> Asm v2
(>>) :: Asm () -> Asm v2 -> Asm v2
fail :: Asm v
mfix :: (v -> Asm v) -> Asm v

allocateZP :: Asm Word8
label :: Asm Word16

equb :: [Word8] -> Asm ()
equs :: String -> Asm ()

lda_i :: Word8 -> Asm ()
lda_i_char :: Char -> Asm ()
lda_my :: Word16 -> Asm ()
ldy_i :: Word8 -> Asm ()

sta_z :: Word8 -> Asm ()

jsr :: Word16 -> Asm ()
jmp :: Word16 -> Asm ()

beq :: Word16 -> Asm ()
bne :: Word16 -> Asm ()

iny :: Asm ()

lo :: Word16 -> Word8
hi :: Word16 -> Word8


-- implementation
return = pure
pure = Pure
(>>=) = Bind
(>>) asm1 asm2 = asm1 >>= \() -> asm2
fail = error "UntypedAsm.fail"
mfix = Mfix

allocateZP = AllocateZP
label = Label

equb = Emit
equs str = Emit (map c2w str)

lda_i b = Emit [0xa9, b]
lda_i_char c = Emit [0xa9, c2w c]
lda_my a = Emit [0xb9, lo a, hi a]
ldy_i b = Emit [0xa0, b]

sta_z b = Emit [0x85, b]

jsr a = Emit [0x20, lo a, hi a]
jmp a = Emit [0x4c, lo a, hi a]

beq = branch 0xf0
bne = branch 0xd0

iny = Emit [0xc8]

lo w = fromIntegral (w .&. 0xff)
hi w = fromIntegral (w `shiftR` 8)

branch :: Word8 -> Word16 -> Asm ()
branch opcode a =
  Label >>= \here -> Emit [opcode, fromIntegral (a - here - 2) ]

-- assemble
data Asm v where
  Pure :: v -> Asm v
  Bind :: Asm v -> (v -> Asm w) -> Asm w
  Emit :: [Word8] -> Asm ()
  Label :: Asm Word16
  Mfix :: (a -> Asm a) -> Asm a
  AllocateZP :: Asm Word8

assemble :: Word16 -> Asm () -> [Word8]
assemble origin m0 = do
  let (_, (), bytes) = loop State {at = origin, zp = 0x70} m0 in bytes
  where
    loop :: State -> Asm a -> (State, a, [Word8])
    loop s m0 = case m0 of
      Pure v -> (s,v,[])
      Bind m f ->
        case loop s m of
          (s,v,bs1) ->
            case loop s (f v) of
              (s,w,bs2) ->
                (s, w, bs1 ++ bs2)
      Mfix g -> do
        let x@(_, a,_) = loop s (g a)
        x
      Emit ws ->
        (s { at = fromIntegral (length ws) + at s }, (), ws)
      Label ->
        (s, at,[]) where State{at} = s
      AllocateZP ->
        (s { zp = 1 + zp }, zp,[]) where State{zp} = s

data State = State { at :: Word16, zp :: Word8 }
