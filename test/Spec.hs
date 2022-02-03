{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Plutarch (printScript, printTerm)
import Plutus.V1.Ledger.Scripts (fromCompiledCode)
import PlutusTx (CompiledCode)
import PlutusTx qualified
import Test.Tasty
import Test.Tasty.HUnit ((@?=), testCase)
import Plutarch.Prelude
import PlutusTx.Prelude
import Prelude (IO, String)
import Prelude qualified

-- | @since 0.1
main :: IO ()
main = defaultMain tests

printCode :: CompiledCode a -> String
printCode = printScript . fromCompiledCode

double :: CompiledCode (Integer -> Integer)
double = $$(PlutusTx.compile [|| (2 *) :: Integer -> Integer ||])

{- | Project wide tests

 @since 0.1
-}
tests :: TestTree
tests =
  testGroup
    "Test.PlutarchFFI"
    [ testCase "integer literal" $ printCode $$(PlutusTx.compile [|| 42 :: Integer ||]) @?= "(program 1.0.0 42)"
    , testCase "PlutusTx integer function" $ printCode double @?= "(program 1.0.0 (\\i0 -> multiplyInteger 2 i1))"
    , testCase "Plutarch integer function" $ printTerm (plam $ \(x :: Term _ PInteger)-> 2 Prelude.* x)
      @?= "(program 1.0.0 (\\i0 -> multiplyInteger 2 i1))"
    ]
