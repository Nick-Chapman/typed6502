
module Effect where

import Data.Kind (Type)

data VAL = Value Type | ReturnAddr CPU
data STACK = Cons { _head :: VAL, _tail :: STACK }
data CPU = Cpu { _acc :: VAL, _xreg :: VAL, _yreg :: VAL, _stack :: STACK }
data GENERATED = Data VAL | Code { _cpu :: CPU }

type State (a::VAL) (x::VAL) (y::VAL) (s::STACK) = 'Code ('Cpu a x y s)
