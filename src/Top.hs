
module Top where

import Asm

main :: IO ()
main = do
  putStrLn "*typed-6502*"

swapXY :: Asm (State a x y s) (State a y x s) ()
swapXY = Asm.mdo
  pha
  txa
  pha
  tya
  tax
  pla
  tay
  pla

swapMacroTwice :: Asm (State a x y s) (State a x y s) ()
swapMacroTwice = Asm.do
  swapXY
  swapXY

swapRoutineTwice :: Asm (State a x y s) (State a x y s) ()
swapRoutineTwice = Asm.mdo

  jmp main
  r1 <- labelCode
  swapXY
  rts

  r2 <- labelCode
  swapXY
  rts

  main <- labelCode
  jsr r1
  jsr r2


data Speed
data Lives

prog1 :: Asm (State ('Value Lives) x y s) (State ('Value Speed) x y s) ()
prog1 = Asm.mdo
  vSpeed <- allocateZP @('Value Speed)
  vLives <- allocateZP @('Value Lives)
  staZ vLives
  ldaZ vSpeed


prog2 :: Asm (State a x y s) (State a x y s) ()
prog2 = Asm.mdo
  pha; txa; pha
  tax
  pla; tax; pla


prog3 :: Asm (State a x y s) 'NotExecutable ()
prog3 = Asm.mdo
  vLives <- allocateZP @('Value Lives)
  jmp main

  getCounter <- labelCode
  ldaZ vLives
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
