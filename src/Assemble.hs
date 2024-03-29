
module Assemble (Asm(..), assemble) where

import Data.Word (Word8,Word16)
import Effect (VAL)

data Asm ( pre :: VAL) ( post :: VAL) v where
  Pure :: v -> Asm i i v
  Bind :: Asm i j v -> (v -> Asm j k w) -> Asm i k w
  Emit :: [Word8] -> Asm i j ()
  Label :: Asm i j Word16
  Mfix :: (a -> Asm i j a) -> Asm i j a
  AllocateZP :: Asm i j Word8

assemble :: Word16 -> Asm i j () -> [Word8]
assemble origin m0 = do
  let (_, (), bytes) = loop State {at = origin, zp = 0x70} m0 in bytes
  where
    loop :: State -> Asm i j a -> (State, a, [Word8])
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
