
module Effect where

import Data.Kind (Type)

data VAL = Data Type | ReturnAddr CPU | Code CPU
data STACK = Cons { _head :: VAL, _tail :: STACK }
data CPU = Cpu { _acc :: VAL, _xreg :: VAL, _yreg :: VAL, _stack :: STACK }

type State (a::VAL) (x::VAL) (y::VAL) (s::STACK) = 'Code ('Cpu a x y s)
