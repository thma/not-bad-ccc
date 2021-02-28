module InterpreterSpec where

import           Test.Hspec
import           Test.QuickCheck
-- import           Test.QuickCheck.Property as P

import Interpreter
import Rewrite
import FreeCat
import CCC

idCCC :: FreeCat Int Int
idCCC = simplify . toCCC $ id

addCCC :: FreeCat (Integer, Integer) Integer
addCCC = simplify . toCCC $ uncurry (+)

mulCCC :: FreeCat (Double, Double) Double
mulCCC = simplify . toCCC $ uncurry (*)

subCCC :: FreeCat (Integer, Integer) Integer
subCCC = simplify . toCCC $ uncurry (-)

negCCC :: FreeCat Int Int
negCCC = simplify . toCCC $ negate

absCCC :: FreeCat Float Float
absCCC = simplify . toCCC $ abs

example6 :: FreeCat (Integer, Integer) (Integer, Integer)
example6 = simplify $ toCCC (\(x, y) -> (y + (x * y), x * y))

spec :: Spec
spec = do
  describe "The CCC Interpreter" $ do
    it "interpretes the id function" $
      property $ \x -> interp idCCC x `shouldBe` x
    it "interpretes addition" $
      property $ \x y -> interp addCCC (x,y) `shouldBe` x+y
    it "interpretes multiplication" $
      property $ \x y -> interp mulCCC (x,y) `shouldBe` x*y
    it "interpretes substraction" $
      property $ \x y -> interp subCCC (x,y) `shouldBe` x-y      
    it "interpretes negation" $
      property $ \x -> interp negCCC x `shouldBe` negate x
    it "interpretes absolute" $
      property $ \x -> interp absCCC x `shouldBe` abs x
    it "interpretes combination of + and *" $
      property $ \x y -> interp example6 (x,y) `shouldBe` (\(a, b) -> (b + (a * b), a * b)) (x,y)       