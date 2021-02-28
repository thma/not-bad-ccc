module InterpreterSpec where

import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Property as P

import Interpreter
import Rewrite
import FreeCat
import CCC

idCCC :: FreeCat Int Int
idCCC = simplify $ toCCC id

spec :: Spec
spec = do
  describe "The CCC Interpreter" $ do
    it "interpretes the id function" $
      property (\x -> interp idCCC x `shouldBe` x)