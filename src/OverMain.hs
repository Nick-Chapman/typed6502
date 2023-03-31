
{-# LANGUAGE RebindableSyntax #-}
--{-# OPTIONS -Wno-missing-signatures #-}

module OverMain (main) where

import Data.Word8 (Word8)
import Prelude hiding (return,pure,(>>=),(>>))
import OverThink

main = eff $ do
  v1 <- AllocZP
  _v2 <- AllocZP
  lda_i (immChar 'x')
  sta_z v1
  lda_i (immWord 123)
  sta_z (nextZ v1) -- v1 is a type error, nice!

  pure ()


eff :: Asm g ('Gen z ('Seq ('Code cpu op cpu2) h)) v -> Effect cpu v
eff = undefined
data Effect (cpu :: CpuState) v


--zpType :: Byte ('ZpAddr ('Seq i is)) -> Byte i
--zpType = undefined

_w :: Word8
_w = undefined
