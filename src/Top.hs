
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RebindableSyntax #-}

module Top (main,prog1,prog2) where

import Asm
import Prelude hiding (return,pure,(>>=),(>>))

main :: IO ()
main = do
  putStrLn "*typed-6502*"

data Counter

prog1 :: Generated ('E ('S a x y s) ('S ('Value Counter) x y s))
prog1 = assemble $ mdo
  vCounter <- allocateZP @ ('Value Counter)
  jmp main

  getCounter <- labelCode
  ldaZP vCounter
  rts

  spin <- labelCode
  pha
  php
  pla
  pla
  jmp spin

  main <- labelCode
  jsr getCounter
  jmp spin


prog2 :: Generated ('E ('S a x y s) ('S a y x s))
prog2 = assemble $ mdo
  jsr swapXY
  halt

  swapXY <- labelCode
  pha
  txa
  pha
  tya
  tax
  pla
  tay
  pla
  rts
