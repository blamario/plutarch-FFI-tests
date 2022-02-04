{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Plutarch (printScript, printTerm)
import Plutarch.FFI (foreignExport, foreignImport)
import Plutarch.Prelude
import Plutarch.Rec qualified as Rec
import Plutarch.Rec.TH (deriveAll)
import Plutus.V1.Ledger.Scripts (fromCompiledCode)
import PlutusTx (CompiledCode, applyCode)
import PlutusTx qualified
import PlutusTx.Prelude
import Test.Tasty
import Test.Tasty.HUnit (testCase, (@?=))
import Prelude (IO, String)
import Prelude qualified

printCode :: CompiledCode a -> String
printCode = printScript . fromCompiledCode

double :: CompiledCode (Integer -> Integer)
double = $$(PlutusTx.compile [||(2 *) :: Integer -> Integer||])

doubleImported :: Term s (PInteger :--> PInteger)
doubleImported = foreignImport double

doubleExported :: CompiledCode (Integer -> Integer)
doubleExported = foreignExport (plam $ \(x :: Term _ PInteger) -> 2 Prelude.* x)

data SampleRecord = SampleRecord
  { sampleBool :: Bool
  , sampleInt :: Integer
  , sampleString :: BuiltinString
  }

data PSampleRecord f = PSampleRecord
  { psampleBool :: f PBool
  , psampleInt :: f PInteger
  , psampleString :: f PString
  }
$(deriveAll ''PSampleRecord)

-- | @since 0.1
main :: IO ()
main = defaultMain tests

{- | Project wide tests

 @since 0.1
-}
tests :: TestTree
tests =
  testGroup
    "Test.PlutarchFFI"
    [ testGroup
      "Simple types"
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
    , testGroup
      "Records"
      [ testCase "PlutusTx record value" $
        printCode $$(PlutusTx.compile [|| SampleRecord False 6 "Hello" ||]) @?= sampleScottEncoding
      , testCase "Plutarch record value" $
        printTerm (Rec.rcon $ PSampleRecord (pcon PFalse) 6 "Hello") @?= "(program 1.0.0 (\\i0 -> i1 False 6 \"Hello\"))"
      , testCase "PlutusTx record function" $
        printCode $$(PlutusTx.compile [|| sampleInt ||]) @?= sampleScottField
      , testCase "Plutarch record function" $
        printTerm (plam $ \r-> r # Rec.field psampleInt) @?= "(program 1.0.0 (\\i0 -> i1 (\\i0 -> \\i0 -> \\i0 -> i2)))"
      ]
    ]
    where
      sampleScottEncoding = "(program 1.0.0 ((\\i0 -> \\i0 -> \\i0 -> (\\i0 -> \\i0 -> i2 i4 6 \"Hello\") (\\i0 -> \\i0 -> \\i0 -> delay (\\i0 -> i1 i4 i3 i2)) (\\i0 -> i1)) (delay (\\i0 -> \\i0 -> i2)) (delay (\\i0 -> \\i0 -> i1)) (\\i0 -> i1)))"
      sampleScottField = "(program 1.0.0 ((\\i0 -> \\i0 -> \\i0 -> force (i2 i1) (\\i0 -> \\i0 -> \\i0 -> i2)) (\\i0 -> \\i0 -> \\i0 -> delay (\\i0 -> i1 i4 i3 i2)) (\\i0 -> i1)))"
