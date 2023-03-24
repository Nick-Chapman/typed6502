
module Asm
  ( Asm, assemble, writeBytes
  , pure, (>>=), (>>), return, mfix, fail
  , equbs
  , lda_i_char
  , jsr, jmp, label
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

pure :: v -> Asm v
return :: v -> Asm v
mfix :: (v -> Asm v) -> Asm v
fail :: Asm v
(>>=) :: Asm v1 -> (v1 -> Asm v2) -> Asm v2
(>>) :: Asm () -> Asm v2 -> Asm v2
equbs :: [Word8] -> Asm ()
lda_i_char :: Char -> Asm ()
jsr :: Word16 -> Asm ()
jmp :: Word16 -> Asm ()
label :: Asm Word16

-- implementation
return = pure
(>>) asm1 asm2 = asm1 >>= \() -> asm2
pure = A_Pure
(>>=) = A_Bind
mfix = undefined -- TODO:
fail = undefined
equbs = A_Emit
lda_i_char c = A_Emit [0xa9, c2w c]
jsr a = A_Emit [0x20, lo a, hi a]
jmp a = A_Emit [0x4c, lo a, hi a]
label = A_Label

-- assemble
data Asm v where
  A_Pure :: v -> Asm v
  A_Bind :: Asm v -> (v -> Asm w) -> Asm w
  A_Emit :: [Word8] -> Asm ()
  A_Label :: Asm Word16

assemble :: Word16 -> Asm () -> [Word8]
assemble origin m = loop m State{ at = origin } (\_ () -> [])
  where
    loop :: Asm a -> State -> (State -> a -> [Word8]) -> [Word8]
    loop m s k = case m of
      A_Pure v ->
        k s v
      A_Bind m f ->
        loop m s $ \s a -> loop (f a) s k
      A_Emit ws ->
        ws ++ k s { at = fromIntegral (length ws) + at } () where State{at} = s
      A_Label ->
        k s at where State{at} = s

data State = State { at :: Word16 }

lo :: Word16 -> Word8
lo w = fromIntegral (w .&. 0xff)

hi :: Word16 -> Word8
hi w = fromIntegral (w `shiftR` 8)
