{-# OPTIONS -Wno-unticked-promoted-constructors #-}

module PlayTypeFamilies where

import Data.Kind (Type)
import Data.Word (Word8,Word16)

data B = T | F

myNot :: B -> B
myNot F = T
myNot T = F

type MyNot :: B -> B
type family MyNot b where
  MyNot T = F
  MyNot F = T

type MyNotOpen :: B -> B
type family MyNotOpen b
type instance MyNotOpen T = F
type instance MyNotOpen F = T


data Mode = Immediate | Absolute | ZeroPage

type ModeRep :: Mode -> Type
type family ModeRep m where
  ModeRep Immediate = Word8
  ModeRep Absolute = Word16


data CpuState =
  Cpu { _a :: Interpretation
      , _x :: Interpretation
      , _y :: Interpretation
      }

data Interpretation
  = Data Type
  | Code CpuState

data CpuEffect = Eff CpuState CpuState


type From :: CpuEffect -> CpuState
type family From e where
  From (Eff c d) = c

type CpuEffect2 = (CpuState,CpuState)
--type From2 :: CpuEffect2 -> CpuState

type From2 :: (CpuState,CpuState) -> CpuState
type family From2 e where
  From2 '(c,d) = c
