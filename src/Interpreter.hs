{-# LANGUAGE GADTs #-}

{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Interpreter where

import           Cat  
import           FreeCat
import           Data.Bifunctor   (bimap)  
import Prelude hiding (EQ)

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
  eqC  = uncurry (==)


red term arg = 
  let step = interp term arg
   in step

interp :: FreeCat a b -> (a -> b)
interp (Comp f g) = interp f . interp g
interp (Par f g)  = parC (interp f) (interp g)
interp (Curry f)  = Wrap . curry (interp f)
interp (Uncurry f) = error "not yet implemented" -- _f $ interp f 
interp Apply      = uncurry interp  
interp Id         = id
interp Fst        = fst
interp Snd        = snd
interp Dup        = dupC
interp Add        = addC
interp Sub        = subC
interp Abs        = abs
interp Neg        = negate
interp AddCurry   = \x -> Wrap (x +)
interp Mul        = mulC
interp (Wrap f)   = f
interp EQ         = eqC

 

--Par :: FreeCat a b -> FreeCat c d -> FreeCat (a, c) (b, d)
--Apply :: FreeCat (FreeCat a b, a) b
-- interp Apply (Add, (3, 5)) --> 8

--Curry :: FreeCat (a, b) c -> FreeCat a (FreeCat b c)
--Uncurry :: FreeCat a (FreeCat b c) -> FreeCat (a, b) c