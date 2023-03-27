
module Fragments (top) where

import Prelude hiding (pure)
import Asm
--import Data.Word (Word8)

top :: ()
top = ()
  where

    _frag1 = Asm.do
      v <- allocateZP
      lda (immChar 'c')
      sta_z v
      --lda (immediate 11) -- TEST: this should be a type error
      sta_z v

