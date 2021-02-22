{-# LANGUAGE GADTs #-}

module Interpreter where
  
import           Cat  
import           FreeCat
import           Data.Bifunctor   (bimap)  

instance Monoidal (->) where
  parC f g = bimap f g

instance Cartesian (->) where
  fstC (x, y) = x
  sndC (x, y) = y
  dupC x = (x, x)
  
instance Closed (->) where
    applyC (f,x) = f x
    curryC       = curry
    uncurryC     = uncurry   

instance NumCat (->) where
  mulC = uncurry (*)
  negateC = negate
  addC = uncurry (+)
  subC = uncurry (-)
  absC = abs

interp :: FreeCat a b -> (a -> b)
interp (Comp f g)  = interp f . interp g
interp (Par f g)   = parC (interp f) (interp g)
--interp (Curry f)   = (interp f) --interp $ curryC _f
interp (Uncurry f) = interp $ uncurryC f
interp Apply       = uncurryC interp
interp Id          = idC
interp Fst         = fstC
interp Snd         = sndC
interp Dup         = dupC
interp Add         = addC
interp Mul         = mulC

--Par :: FreeCat a b -> FreeCat c d -> FreeCat (a, c) (b, d)
--Apply :: FreeCat (FreeCat a b, a) b
--Curry :: FreeCat (a, b) c -> FreeCat a (FreeCat b c)
--Uncurry :: FreeCat a (FreeCat b c) -> FreeCat (a, b) c