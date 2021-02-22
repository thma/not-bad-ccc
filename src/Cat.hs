{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Cat where

import           Control.Category
import           Prelude          hiding (id, (.))

class Category k => Monoidal k where
  parC :: k a c -> k b d -> k (a, b) (c, d)

class Monoidal k => Cartesian k where
  fstC :: k (a, b) a
  sndC :: k (a, b) b
  dupC :: k a (a, a)

class Cartesian k => Closed k where
  applyC :: k (k a b, a) b
  curryC :: k (a, b) c -> k a (k b c)
  uncurryC :: k a (k b c) -> k (a, b) c

fanC :: Cartesian cat => cat b c -> cat b d -> cat b (c, d)
fanC f g = parC f g . dupC

idC :: Category k => k a a
idC = id

class Cartesian k => NumCat k where
  mulC :: Num a => k (a, a) a
  negateC :: Num a => k a a
  addC :: Num a => k (a, a) a
  subC :: Num a => k (a, a) a
  absC :: Num a => k a a


{-
instance (NumCat k, Num a) => Num (k z a) where
    f + g = addC . (fanC f g)
    f * g = mulC . (fanC f g)
    negate f = negateC . f
    f - g = subC . (fanC f g)
    abs f = absC . f
    signum = error "TODO"
    fromInteger = error "TODO"
-}

{-
class BoolLike a where
    (&&) :: a -> a -> a
    (||) :: a -> a -> a
    not :: a -> a
    ite :: a -> b -> c -> Either b c

-}