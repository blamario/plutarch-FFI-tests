{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch (ClosedTerm, compile, printScript, printTerm)
import Plutarch.Api.V1 (PPubKeyHash, PScriptContext, PTxInfo)
import Plutarch.Evaluate (evaluateScript)
import Plutarch.FFI (foreignExport, foreignImport)
import Plutarch.Prelude
import Plutarch.Rec qualified as Rec
import Plutarch.Rec.TH (deriveAll)
import Plutus.V1.Ledger.Api (
  Address (Address),
  Credential (ScriptCredential),
  CurrencySymbol,
  DatumHash,
  PubKeyHash,
  ScriptContext (ScriptContext),
  ScriptPurpose (Spending),
  TxInInfo (TxInInfo, txInInfoOutRef, txInInfoResolved),
  TxInfo (
    TxInfo,
    txInfoDCert,
    txInfoData,
    txInfoFee,
    txInfoId,
    txInfoInputs,
    txInfoMint,
    txInfoOutputs,
    txInfoSignatories,
    txInfoValidRange,
    txInfoWdrl
  ),
  TxOut (TxOut, txOutAddress, txOutDatumHash, txOutValue),
  TxOutRef (TxOutRef),
  ValidatorHash,
  Value,
  getTxId,
 )
import Plutus.V1.Ledger.Contexts qualified as Contexts
import Plutus.V1.Ledger.Interval qualified as Interval
import Plutus.V1.Ledger.Scripts (ScriptError, fromCompiledCode)
import Plutus.V1.Ledger.Value qualified as Value
import PlutusTx (CompiledCode, applyCode)
import PlutusTx qualified
import PlutusTx.Builtins.Internal (BuiltinBool)
import PlutusTx.Prelude
import Shrink (shrinkScript, shrinkScriptSp, withoutTactics)
import Test.Tasty
import Test.Tasty.HUnit (testCase, (@?=))

-- import Test.Tasty.Plutus.Internal.Context (ContextBuilder (cbSignatories), TransactionConfig(..), compileSpending)
import Prelude (IO, String)
import Prelude qualified

printCode :: CompiledCode a -> String
printCode = printScript . fromCompiledCode

printShrunkCode :: CompiledCode a -> String
printShrunkCode = printScript . shrink . shrink . shrink . fromCompiledCode
  where
    shrink = shrinkScriptSp (withoutTactics ["strongUnsubs", "weakUnsubs"])

printEvaluatedCode :: CompiledCode a -> Either ScriptError String
printEvaluatedCode = fmap (printScript . lastOf3) . evaluateScript . fromCompiledCode

printShrunkTerm :: ClosedTerm a -> String
printShrunkTerm x = printScript $ shrinkScript $ compile x

printEvaluatedTerm :: ClosedTerm a -> Either ScriptError String
printEvaluatedTerm s = fmap (printScript . lastOf3) . evaluateScript $ compile s

lastOf3 :: (_, _, a) -> a
lastOf3 (_, _, x) = x

double :: CompiledCode (Integer -> Integer)
double = $$(PlutusTx.compile [||(2 *) :: Integer -> Integer||])

doubleImported :: Term s (PInteger :--> PInteger)
doubleImported = foreignImport double

doubleExported :: CompiledCode (Integer -> Integer)
doubleExported = foreignExport (plam $ \(x :: Term _ PInteger) -> 2 Prelude.* x)

data SampleRecord = SampleRecord
  { sampleBool :: BuiltinBool
  , sampleInt :: Integer
  , sampleString :: BuiltinString
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic)

data PSampleRecord f = PSampleRecord
  { psampleBool :: f PBool
  , psampleInt :: f PInteger
  , psampleString :: f PString
  }
$(deriveAll ''PSampleRecord)

importedField :: Term _ (PDelayed (Rec.PRecord PSampleRecord) :--> PInteger)
importedField = foreignImport ($$(PlutusTx.compile [||sampleInt||]) :: CompiledCode (SampleRecord -> Integer))

exportedField :: CompiledCode (SampleRecord -> Integer)
exportedField =
  foreignExport
    ( (plam $ \r -> pmatch (pforce r) $ \(Rec.PRecord rr) -> psampleInt rr) ::
        Term _ (PDelayed (Rec.PRecord PSampleRecord) :--> PInteger)
    )

getTxInfo :: Term _ (PAsData PScriptContext :--> PAsData PTxInfo)
getTxInfo = pfield @"txInfo"

exportedTxInfo :: CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData)
exportedTxInfo = foreignExport getTxInfo

importedTxSignedBy :: Term _ (PAsData PTxInfo :--> PAsData PPubKeyHash :--> PBool)
importedTxSignedBy = foreignImport $$(PlutusTx.compile [||txDataSignedBy||])
  where
    txDataSignedBy :: BuiltinData -> BuiltinData -> BuiltinBool
    txDataSignedBy tx pkh = toBuiltin $ any id (Contexts.txSignedBy <$> PlutusTx.fromBuiltinData tx <*> PlutusTx.fromBuiltinData pkh)

---- lifted from https://github.com/Plutonomicon/plutarch/blob/master/examples/Examples/Api.hs ----

{- |
  An example 'PScriptContext' Term,
  lifted with 'pconstant'
-}
ctx :: ScriptContext
ctx = ScriptContext info purpose

-- | Simple script context, with minting and a single input
info :: TxInfo
info =
  TxInfo
    { txInfoInputs = [inp]
    , txInfoOutputs = []
    , txInfoFee = mempty
    , txInfoMint = mint
    , txInfoDCert = []
    , txInfoWdrl = []
    , txInfoValidRange = Interval.always
    , txInfoSignatories = signatories
    , txInfoData = []
    , txInfoId = "b0"
    }

-- | A script input
inp :: TxInInfo
inp =
  TxInInfo
    { txInInfoOutRef = ref
    , txInInfoResolved =
        TxOut
          { txOutAddress =
              Address (ScriptCredential validator) Nothing
          , txOutValue = mempty
          , txOutDatumHash = Just datum
          }
    }

-- | Minting a single token
mint :: Value
mint = Value.singleton sym "sometoken" 1

ref :: TxOutRef
ref = TxOutRef "a0" 0

purpose :: ScriptPurpose
purpose = Spending ref

validator :: ValidatorHash
validator = "a1"

datum :: DatumHash
datum = "d0"

sym :: CurrencySymbol
sym = "c0"

signatories :: [PubKeyHash]
signatories = ["ab01fe235c", "123014", "abcdef"]

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
            printTerm (plam $ \n -> doubleImported #$ doubleImported # n)
              @?= "(program 1.0.0 (\\i0 -> (\\i0 -> multiplyInteger 2 i1) (multiplyInteger 2 i1)))"
        , testCase "Exported and applied Plutarch integer function" $
            printCode (doubleExported `applyCode` $$(PlutusTx.compile [||21 :: Integer||]))
              @?= "(program 1.0.0 ((\\i0 -> multiplyInteger 2 i1) 21))"
        , testCase "Bool->Integer in Plutarch" $
            printShrunkTerm (plam $ \x -> pif x (1 :: Term _ PInteger) 0)
              @?= "(program 1.0.0 (\\i0 -> force (force ifThenElse i1 (delay 1) (delay 0))))"
        , testCase "Bool->Integer in PlutusTx" $
            printShrunkCode $$(PlutusTx.compile [||\x -> if x then 1 :: Integer else 0||])
              @?= "(program 1.0.0 (\\i0 -> force i1 1 0))"
        ]
    , testGroup
        "Records"
        [ testCase "PlutusTx record value" $
            printShrunkCode $$(PlutusTx.compile [||SampleRecord (toBuiltin False) 6 "Hello"||]) @?= sampleScottEncoding
        , testCase "Plutarch record value" $
            printTerm (pdelay $ Rec.rcon $ PSampleRecord (pcon PFalse) 6 "Hello") @?= sampleScottEncoding
        , testCase "PlutusTx record function" $
            printShrunkCode $$(PlutusTx.compile [||sampleInt||]) @?= sampleScottField
        , testCase "Plutarch record function" $
            printTerm (plam $ \r -> pforce r # Rec.field psampleInt) @?= sampleScottField
        , testCase "Apply PlutusTx record function in Plutarch" $
            printShrunkTerm (importedField #$ pdelay $ pcon $ Rec.PRecord $ PSampleRecord (pcon PFalse) 6 "Hello") @?= "(program 1.0.0 6)"
        , testCase "Apply Plutarch record function in PlutusTx" $
            printShrunkCode (exportedField `applyCode` $$(PlutusTx.compile [||SampleRecord (toBuiltin False) 6 "Hello"||]))
              @?= "(program 1.0.0 6)"
        ]
    , testGroup
        "Data"
        [ testGroup
            "Export and use a PData :--> PData function"
            [ testCase "evaluate a field" $
                printEvaluatedCode
                  ( $$(PlutusTx.compile [||\gti ctx -> maybe "undecoded" (getTxId . txInfoId) (PlutusTx.fromBuiltinData (gti ctx))||])
                      `applyCode` exportedTxInfo
                      `applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData ctx)
                  )
                  @?= Right "(program 1.0.0 #b0)"
            , testCase "evaluate a function to True" $
                printEvaluatedCode
                  ( $$(PlutusTx.compile [||\gti ctx pkh -> maybe False (`Contexts.txSignedBy` pkh) (PlutusTx.fromBuiltinData (gti ctx))||])
                      `applyCode` exportedTxInfo
                      `applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData ctx)
                      `applyCode` PlutusTx.liftCode (head signatories)
                  )
                  @?= Right "(program 1.0.0 (delay (\\i0 -> \\i0 -> i2)))"
            , testCase "evaluate a function to False" $
                printEvaluatedCode
                  ( $$(PlutusTx.compile [||\gti ctx pkh -> maybe False (`Contexts.txSignedBy` pkh) (PlutusTx.fromBuiltinData (gti ctx))||])
                      `applyCode` exportedTxInfo
                      `applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData ctx)
                      `applyCode` PlutusTx.liftCode "0123"
                  )
                  @?= Right "(program 1.0.0 (delay (\\i0 -> \\i0 -> i1)))"
            ]
        , testGroup
            "Import and use a BuiltinData -> x function"
            [ testCase "evaluate a function to True" $
                printEvaluatedTerm (importedTxSignedBy # pconstantData info # pconstantData (head signatories))
                  @?= Right "(program 1.0.0 True)"
            , testCase "evaluate a function to False" $
                printEvaluatedTerm (importedTxSignedBy # pconstantData info # pconstantData "0123")
                  @?= Right "(program 1.0.0 False)"
            ]
        ]
    ]
  where
    sampleScottEncoding = "(program 1.0.0 (delay (\\i0 -> i1 False 6 \"Hello\")))"
    sampleScottField = "(program 1.0.0 (\\i0 -> force i1 (\\i0 -> \\i0 -> \\i0 -> i2)))"
