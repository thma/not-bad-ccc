{-# LANGUAGE GADTs #-}

{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Interpreter where

import           Cat  
import           FreeCat
import           Hask () -- this imports instance declaration for (->) which is needed by @interp


  
instance BoolLike Bool where
  (&&) = (Prelude.&&)
  (||) = (Prelude.||)
  not  = Prelude.not
  ite test (thenPart, elsePart) = if test then thenPart else elsePart
  
fix :: (a -> a) -> a
fix f = let {x = f x} in x
  
red :: FreeCat a1 (a2 -> a2) -> a1 -> a2
red term arg = fix $ interp term arg

interp :: FreeCat a b -> (a -> b)
interp (Comp f g) = interp f . interp g
interp (Par f g)  = parC (interp f) (interp g)
interp (Curry f)  = Wrap . curry (interp f)
interp (Uncurry f) = error "not yet implemented" -- _f (interp f)
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
--interp Eql        = eqlC
interp Leq        = leqC
interp Geq        = geqC
interp Les        = lesC
interp Gre        = greC
interp And        = andC
interp Or         = orC
interp Not        = notC
interp IfThenElse = ifTE
 

--Par :: FreeCat a b -> FreeCat c d -> FreeCat (a, c) (b, d)
--Apply :: FreeCat (FreeCat a b, a) b
-- interp Apply (Add, (3, 5)) --> 8

--Curry :: FreeCat (a, b) c -> FreeCat a (FreeCat b c)
--Uncurry :: FreeCat a (FreeCat b c) -> FreeCat (a, b) c