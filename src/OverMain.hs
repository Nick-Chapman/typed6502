
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS -Wno-missing-signatures #-}

module OverMain (main) where

import Prelude hiding (return,pure,(>>=),(>>))
import OverThink

main = eff $ do
  v <- AllocZP
  lda_i (immChar 'x')
  tax
  lda_i (immWord 123)
  tay
  lda_i v
  pure v

eff :: Asm g ('Gen z ('Seq ('Code cpu op cpu2) h)) v -> Effect cpu v
eff = undefined
data Effect (cpu :: CpuState) v

