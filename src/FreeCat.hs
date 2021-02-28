{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ConstraintKinds #-}

module FreeCat where

import           Cat
import           Control.Category
import           Prelude          hiding (id, (.))
import           Data.Typeable 

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


deriving instance (Typeable a, Typeable b) => Typeable (FreeCat a b)

instance Closed FreeCat where
  applyC = Apply
  curryC = Curry
  uncurryC = Uncurry

-- this little hack is needed to allow auto deriving Show for FreeCat
instance Show  (a -> b) where
  showsPrec _ _ = showString "<function>" -- ++ typeInfo x
--    where
--      typeInfo x = show (typeOf x)
       
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
