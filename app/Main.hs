{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE TypeApplications          #-}

module Main where

import           CCC
import           Cat
import           Control.Category
import           Data.Data
import           Data.Generics.Aliases
import           Prelude               hiding (id, (.))
import           Rewrite

mapTuple :: (Data a, Typeable b) => (b -> b) -> a -> a
mapTuple f = gmapT (mkT f)

showSimplified :: FreeCat a b -> IO ()
showSimplified = print . simplify

compile :: (FreeCat a' a' -> FreeCat a' b) -> FreeCat a' b
compile = simplify . toCCC

interp :: FreeCat a b -> (a -> b)
interp (Comp f g) = interp f . interp g
interp (Par f g) = parC (interp f) (interp g)
interp (Curry f) = error "curry not yet implemented"
--interp (Uncurry f)  error "uncurry not yet implemented"
interp Id  = idC
interp Fst = fstC
interp Snd = sndC
interp Dup = dupC
interp Add = addC
interp Mul = mulC

--Par :: FreeCat a b -> FreeCat c d -> FreeCat (a, c) (b, d)
--Apply :: FreeCat (FreeCat a b, a) b
--Curry :: FreeCat (a, b) c -> FreeCat a (FreeCat b c)
--Uncurry :: FreeCat a (FreeCat b c) -> FreeCat (a, b) c

main :: IO ()
main = do
  showSimplified example1
  showSimplified example2
  showSimplified example3
  showSimplified example4
  showSimplified example5
  showSimplified example6
  showSimplified example7
  showSimplified example8
  showSimplified example9
  showSimplified example10
  showSimplified example11
  showSimplified example12
  showSimplified example13
  showSimplified example14
  showSimplified example15
  showSimplified example16
  showSimplified example17
  showSimplified example18
  showSimplified example19
  showSimplified example21
  showSimplified example22
  showSimplified example23
  showSimplified example24
  showSimplified example25
  showSimplified example26



example2 :: FreeCat (a,b) (b,a)
example2 = toCCC @FreeCat (\(x,y)->(y,x))

-- You need to give the type sginature unfortunately. k is too ambiguous otherwise
-- example3 :: Cartesian k => k _ _
example3 :: FreeCat (b'1, b'2) (b'1, b'1)
example3 = toCCC @FreeCat (\(z,y)->(z,z))

example4 = toCCC @FreeCat (\((x,y),z) -> x)

example5 = toCCC @FreeCat (\(x,y) -> x + y)

example6 = toCCC @FreeCat (\(x,y) -> (y + (x * y), x * y))

-- example7 :: Cartesian k => k _ _
example7 = toCCC @FreeCat (\(x,(y,z)) -> (x,(y,z)))

myconst = \x -> \y -> x
example8 = toCCC @FreeCat  myconst -- const -- (\x -> \y -> x)
example9 =  let f = (\x y -> x) in toCCC @FreeCat f
example10 =  toCCC @FreeCat (\x -> x)
example11 =  toCCC @FreeCat f where f = (\x y -> y)

-- raw const is failing, but when you give it a name it doesn't. Very alarming.
-- This is almost certainly because of something in the Incoherent


--example12 :: Cartesian k => k (k a b) b
example12 =  toCCC @FreeCat ((\x y -> y) :: a -> b -> b)

-- the following incorrectly fails. Early picking of incoherentinstamce seems to send it into case 3 of CCC rather than curry case 2.
-- This isn't producing incorrect code, but it does suck.
-- doesnotwork =  toCCC @FreeCat (\x y -> y)

-- Even this is fine
-- example16 =  toCCC @FreeCat ((\x y -> y) :: _ -> _ -> _)
exmaple12' = toCCC @FreeCat (\x -> (x,x))
example13 = toCCC @FreeCat (\x y -> (x,y))
example14 = toCCC @FreeCat f where f = (\x y z -> z)
example15 = toCCC @FreeCat f where f = (\x y z -> x)

example13' = toCCC (\(x,y) -> y)
example1 = toCCC @FreeCat id

example16 = toCCC @FreeCat (+)

example17 = toCCC @FreeCat (*)
-- fails. appears to be another inocherent hiccup. ($) is weird anyway
-- example18 = toCCC @FreeCat ($)

example18 = toCCC @FreeCat f  where f = \g x -> g x
example19 = toCCC @FreeCat (\(g, x) -> g x)

-- fails confusingly. This might mean something is fundmanetally wrong somehwere.
-- This may be failing because it tries to toCCC const as an itermediate step, which we've already
-- seens is tempermental
-- example20 = toCCC @FreeCat f  where f = (\x -> (x, \y -> x))
helper = (\x -> (x, snd)) -- where f = \y -> y
example20' = toCCC @FreeCat helper -- This one came out Comp (Par Id Id) Dup = Dup
-- That isn't right. It should be Curry (Id) = Curry (Par Id Id)
-- What is this? k a (a, k b b) = Id
-- fanC Id (Curry Snd) = (Par Id (Curry Snd)) .  Dup

-- This was a weird bug due to using id. Which was the catgoerical id
-- No the problme is when I fan, I seperate the two type variables, but they are still connected
-- And I try to unify them into different extractor morphism types.
--- oh dear
-- Fixed with bizarre EitherTreem work around

-- can't tell if this one is correct. It is too big. revisit when I have optimizations
example21 = toCCC @FreeCat f  where f = \h g x -> h g x


-- you can throw catagorocial literals in there if you want
-- Edit: not anymore :( ...
-- Wait, why is this working?
-- And the Num stuff still works.
-- It's because we have no fans.
example22 = toCCC @FreeCat (\x -> Id . x)
-- example22' = toCCC @FreeCat (\x -> (Id . x, Id . x)) -- This doesn't work
example23 = toCCC @FreeCat (\(x,y) -> Dup . x)
-- could define helper functions with preapplied (.). dup = (.) Dup
-- then (\x -> dup x) looks more nautral
example24 = toCCC @FreeCat (\(x,y) -> dup x) where dup = (.) Dup


example25 = toCCC @FreeCat (\(x,y) -> (x,y))
example26 = toCCC @FreeCat (\(x,(y,z)) -> (y,z))
-- example27 = toCCC @FreeCat (
-- or perhaps  f $$ x = applyC . (fanC f x). This makes sense in that f and x are extractors.
-- And then.
-- \x -> mysquare x.

-- this all may be just asking to get confused.



-- we could also compile FreeCat as a seperate language, then dump the output to a file and recompile with ghc. Pretty goofy workflow.
-- we can also perhaps find a way to push to an external solver. That would be prettty cool.

-- We could super optimize functions if we have a cetagory equivalence test. Just enumerate all possible functions and find the bets one that matches.
-- Z3?
-- There might be

-- Other possible heurisitcs:
-- Simulated Annealing Maybe.


-- GLobal optimization:
-- Dynamic Programming?
-- MIP ?
-- CSP ?


