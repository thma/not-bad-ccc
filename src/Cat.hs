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

  eqlC :: (Eq a, BoolLike b)  => k (a,a) b
  leqC :: (Ord a, BoolLike b) => k (a,a) b
  geqC :: (Ord a, BoolLike b) => k (a,a) b
  lesC :: (Ord a, BoolLike b) => k (a,a) b
  greC :: (Ord a, BoolLike b) => k (a,a) b


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

{--
class OpCon op con where
  inOp :: con a && con b |- con (a `op` b)

type OkProd k = OpCon (Prod k) (Ok' k)

-- | Category with product.
class (Category k, OkProd k) => ProductCat k where
  exl :: Ok2 k a b => Prod k a b `k` a
  exr :: Ok2 k a b => Prod k a b `k` b
  dup :: Ok  k a => a `k` Prod k a a
--}

class Cartesian k => BoolCat k where
  andC :: BoolLike a => k (a, a) a
  orC  :: BoolLike a => k (a, a) a
  notC :: BoolLike a => k a a
  ifTE :: BoolLike a => k (a, (b, b)) b

class BoolLike a where
  (&&) :: a -> a -> a
  (||) :: a -> a -> a
  not :: a -> a
  ite :: a -> (b, b) -> b
    
--type BoolOf k = Bool

--type Ok k = Type -> Constraint

--class (Cartesian k, Ok k (BoolOf k)) => BoolCat k where
--  -- type BoolOf k
--  notC :: BoolOf k `k` BoolOf k
--  andC, orC, xorC :: Prod k (BoolOf k) (BoolOf k) `k` BoolOf k
