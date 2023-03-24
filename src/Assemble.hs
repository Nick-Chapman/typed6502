
module Assemble
  ( Asm(..)
  , assemble
  ) where

import Data.Word (Word8,Word16)

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
