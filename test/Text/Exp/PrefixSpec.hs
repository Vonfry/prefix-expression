module Text.Exp.PrefixSpec where

import Test.Hspec
import Text.Exp.Prefix (fromInfix)

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "from infix" $ do
    it "correct" $ do
      fromInfix "!((1 + 3) * 3 - 4 ^ 5)" `shouldBe` Right "! - * + 1 3 3 ^ 4 5"
      fromInfix "+2 * (-1) + 3" `shouldBe` Right "+ * +2 -1 3"
      fromInfix "a + b + c" `shouldBe` Right "+ + a b c"
    it "failed" $ do
      (isLeft $ fromInfix "!((1 + 3) * 3 - 4 ^ 5") `shouldBe` True
      (isLeft $ fromInfix "1 + + 2") `shouldBe` True
      (isLeft $ fromInfix "(1 +) + 2") `shouldBe` True
  where
    isLeft p
        | Left _  <- p  = True
        | otherwise     = False
