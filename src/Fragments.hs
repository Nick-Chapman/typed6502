{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS -Wno-missing-signatures #-}

module Fragments (main) where

import Data.Word8 (Word8) -- so the name in printed inferred types is not qualified
import Prelude hiding (return,pure,(>>=),(>>))
import OverThink

_w :: Word8 -- so the import is not redundant
_w = undefined

eff :: Asm g ('Gen z ('Seq ('Code cpu op cpu2) h)) v -> Effect cpu v
eff = undefined
data Effect (cpu :: CpuState) v

zpType :: Byte ('ZpAddr ('Seq i is)) -> Byte i
zpType = undefined


main c1 = eff $ do
  --c2 <- Label
  lda_i (immChar 'x')
  sta_a c1
  pure c1


_frag2 = eff $ do
  v1 <- AllocZP
  v2 <- AllocZP
  lda_i (immChar 'x')
  sta_z v1
  lda_i (immWord 123)
  sta_z (nextZ v1) -- show access to subsequent addresses
  -- sta_z v1 -- type error NICE
  pure (zpType v2)


_frag1 = eff $ do -- from from original Asm
  v <- AllocZP
  lda_i (immChar 'c')
  sta_z v
  --lda_i (immWord 11) -- type error NICE
  sta_z v



