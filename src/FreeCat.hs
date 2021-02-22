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
  Add :: (Num a) => FreeCat (a, a) a
  Mul :: (Num a) => FreeCat (a, a) a
  Apply :: FreeCat (FreeCat a b, a) b
  Curry :: FreeCat (a, b) c -> FreeCat a (FreeCat b c)
  Uncurry :: FreeCat a (FreeCat b c) -> FreeCat (a, b) c

instance Closed FreeCat where
  applyC = Apply
  curryC = Curry
  uncurryC = Uncurry

deriving instance Show (FreeCat a b)

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
  negateC = error "TODO"
  addC = Add
  subC = error "TODO"
  absC = error "TODO"

instance (Num a) => Num (FreeCat z a) where
  f + g = Add . (fanC f g)
  f * g = Mul . (fanC f g)
  negate f = error "todo" -- negateC . f
  f - g = error " todo " -- subC . (fanC f g)
  abs f = error " todo" -- absC . f
  signum = error "TODO"
  fromInteger = error "TODO"
