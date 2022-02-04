{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Plutarch (printScript, printTerm)
import Plutarch.FFI (foreignExport, foreignImport)
import Plutarch.Prelude
import Plutus.V1.Ledger.Scripts (fromCompiledCode)
import PlutusTx (CompiledCode, applyCode)
import PlutusTx qualified
import PlutusTx.Prelude
import Test.Tasty
import Test.Tasty.HUnit (testCase, (@?=))
import Prelude (IO, String)
import Prelude qualified

-- | @since 0.1
main :: IO ()
main = defaultMain tests

printCode :: CompiledCode a -> String
printCode = printScript . fromCompiledCode

double :: CompiledCode (Integer -> Integer)
double = $$(PlutusTx.compile [||(2 *) :: Integer -> Integer||])

doubleImported :: Term s (PInteger :--> PInteger)
doubleImported = foreignImport double

doubleExported :: CompiledCode (Integer -> Integer)
doubleExported = foreignExport (plam $ \(x :: Term _ PInteger) -> 2 Prelude.* x)

{- | Project wide tests

 @since 0.1
-}
tests :: TestTree
tests =
  testGroup
    "Test.PlutarchFFI"
    [ testCase "integer literal" $
        printCode $$(PlutusTx.compile [||42 :: Integer||]) @?= "(program 1.0.0 42)"
    , testCase "PlutusTx integer function" $
        printCode double @?= "(program 1.0.0 (\\i0 -> multiplyInteger 2 i1))"
    , testCase "Plutarch integer function" $
        printTerm (plam $ \(x :: Term _ PInteger) -> 2 Prelude.* x) @?= "(program 1.0.0 (\\i0 -> multiplyInteger 2 i1))"
    , testCase "Imported PlutusTx integer function" $
        printTerm doubleImported @?= "(program 1.0.0 (\\i0 -> multiplyInteger 2 i1))"
    , testCase "Exported Plutarch integer function" $
        printCode doubleExported @?= "(program 1.0.0 (\\i0 -> multiplyInteger 2 i1))"
    , testCase "Imported and applied PlutusTx integer function" $
        printTerm (plam $ \n-> doubleImported #$ doubleImported # n)
        @?= "(program 1.0.0 (\\i0 -> (\\i0 -> multiplyInteger 2 i1) (multiplyInteger 2 i1)))"
    , testCase "Exported and applied Plutarch integer function" $
        printCode (doubleExported `applyCode` $$(PlutusTx.compile [||21 :: Integer||]))
        @?= "(program 1.0.0 ((\\i0 -> multiplyInteger 2 i1) 21))"
    ]
