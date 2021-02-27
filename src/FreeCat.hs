{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE StandaloneDeriving #-}

module FreeCat where

import           Cat
import           Control.Category
import           Prelude          hiding (id, (.))

data FreeCat a b where
  Comp :: FreeCat b c -> FreeCat a b -> FreeCat a c
  Id :: FreeCat a a
  Fst :: FreeCat (a, b) a
  Snd :: FreeCat (a, b) b
  Dup :: FreeCat a (a, a)
  Par :: FreeCat a b -> FreeCat c d -> FreeCat (a, c) (b, d)
  Add      :: (Num a) => FreeCat (a, a) a
  Sub      :: (Num a) => FreeCat (a, a) a
  AddCurry :: (Num a) => FreeCat a (FreeCat a a)
  Mul :: (Num a) => FreeCat (a, a) a
  Abs :: (Num a) => FreeCat a a
  Neg :: (Num a) => FreeCat a a
  Apply :: FreeCat (FreeCat a b, a) b
  Curry :: FreeCat (a, b) c -> FreeCat a (FreeCat b c)
  Uncurry :: FreeCat a (FreeCat b c) -> FreeCat (a, b) c
  Wrap :: (a -> b) -> FreeCat a b

instance Closed FreeCat where
  applyC = Apply
  curryC = Curry
  uncurryC = Uncurry

instance Show (FreeCat a b) where
  show (Comp f g) = "Comp " ++ show f ++ " " ++ show g
  show (Par f g)  = "Par " ++ show f ++ " " ++ show g
  show (Curry f)  = "Curry " ++ show f
  show (Uncurry f) = "Uncurry" ++ show f 
  show Apply      = "Apply"  
  show Id         = "Id"
  show Fst        = "Fst"
  show Snd        = "Snd"
  show Dup        = "Dup"
  show Add        = "Add"
  show Sub        = "Sub"
  show Abs        = "Abs"
  show Neg        = "Neg"
  show AddCurry   = "AddCurry"
  show Mul        = "Mul"
  show (Wrap f)   = "Wrap (_ ->  _)"

instance Category FreeCat where
  (.) = Comp
  id = Id

instance Monoidal FreeCat where
  parC = Par

instance Cartesian FreeCat where
  fstC = Fst
  sndC = Snd
  dupC = Dup

instance NumCat FreeCat where
  mulC = Mul
  negateC = Neg
  addC = Add
  subC = Sub
  absC = Abs

instance (Num a) => Num (FreeCat z a) where
  f + g = Add . (fanC f g)
  f * g = Mul . (fanC f g)
  negate f = error "todo" -- negateC . f
  f - g = error " todo " -- subC . (fanC f g)
  abs f = error " todo" -- absC . f
  signum = error "TODO"
  fromInteger = error "TODO"
