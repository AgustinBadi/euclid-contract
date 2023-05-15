{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE NumericUnderscores #-}

module Main where

import qualified EuclidContract       as OnChain
import           Prelude              (IO, mconcat)
import           Control.Monad        (replicateM)
import           Plutus.Model         (Ada (Lovelace), DatumMode (InlineDatum),
                                       Run, Tx, TypedValidator (TypedValidator),
                                       UserSpend, ada, adaValue, defaultBabbage, 
                                       mustFail, newUser, payToKey, payToScript, 
                                       spend, spendScript, submitTx, testNoErrors,
                                       toV2, userSpend, utxoAt,
                                       waitUntil, noErrors)
import           Plutus.V2.Ledger.Api (POSIXTime,PubKeyHash, TxOut (txOutValue), TxOutRef, Value)
import           PlutusTx.Prelude     (Eq (..), ($), (&&), (.), Integer, Bool (..), (*), (+), Ord ((>)))
import           Prelude              ((<>))
import           Test.Tasty           (defaultMain, testGroup)
import           Test.QuickCheck
import           Test.Tasty.QuickCheck   as QC (testProperty)


main :: IO ()
main = defaultMain $ do
    testGroup
      "Testing Euclid validator"
      [ bad "[Unit] Invalid datum: The 'a' and 'b' values are equal to zero." $ testContract (0,0) (1,-2,5) 
      , bad "[Unit] Invalid datum: The 'a' value is smaller than 'b'." $ testContract (5,12) (1,-2,5)
      , bad "[Unit] Invalid datum: The 'b' value is zero." $ testContract (12,0) (1,-2,5)
      , bad "[Unit] Invalid redeemer: The bet is wrong" $ testContract (12,5) (2,-2,5)
      , good "[Unit] Valid contract: The datum and bet (redeemer) are correct." $ testContract (12,5) (1,-2,5)
      , testProperty "[Property] Valid function: The gdc(a,b) function satisfies gdc == s * a + t * b " checkGdc
      ]
    where
      bad msg = good msg . mustFail
      good = testNoErrors (adaValue 100_000_000_000) defaultBabbage

-- Set many users at once
setupUsers :: Run [PubKeyHash]
setupUsers = replicateM 2 $ newUser $ ada (Lovelace 10000000)

-- Validator's script
valScript :: TypedValidator datum redeemer
valScript = TypedValidator $ toV2 OnChain.validator

waitBeforeConsumingTx :: POSIXTime
waitBeforeConsumingTx = 1000

-- Create transaction that spends "usp" to lock "val" in "valScript"
lockingTx :: (Integer,Integer) -> UserSpend -> Value -> Tx
lockingTx dat usp val =
  mconcat
    [ userSpend usp
    , payToScript valScript (InlineDatum dat) (adaValue 0 <> val)
    ]

-- Create transaction that spends "ref" to unlock "val" from the "valScript" validator
consumingTx :: (Integer,Integer) -> (Integer,Integer,Integer) -> PubKeyHash -> TxOutRef -> Value -> Tx
consumingTx dat redeemer usr ref val =
  mconcat
    [ spendScript valScript ref redeemer dat
    , payToKey usr val
    ]

---------------------------------------------------------------------------------------------------
------------------------------------- TESTING PROPERTIES ------------------------------------------

-- Checks wether the gdc function satisfies gdc(a,b) == s * a + t * b
checkGdc :: Integer -> Integer -> Property
checkGdc a b = 
  ( a > b && b > 0) ==>
  r == (s) * a + (t) * b 
 where 
    (r,s,t) = (OnChain.gdc a b 1 0 0 1)

---------------------------------------------------------------------------------------------------
------------------------------------- RUNNING THE TESTS -------------------------------------------

{--
-- | Check that the expected and real balances match after using the validator with different redeemers
runChecks :: Bool -> (Integer, Integer) -> (Integer, Integer, Integer) -> Property
runChecks shouldConsume datum redeemer = 
  collect (datum,redeemer)  $ monadic property check
    where check = do
            correctContractSpending <- run $ testValues testContract deadline redeemer
            assert correctContractSpending
--}

testContract :: (Integer, Integer) -> (Integer, Integer, Integer) -> Run Bool
testContract dat rdm = do
  -- SETUP USERS
  [u1, u2] <- setupUsers
  -- USER 1 LOCKS 100 Lovelaces ("val") IN VALIDATOR
  let val = adaValue 100                    -- Define value to be transfered
  sp <- spend u1 val                        -- Get user's UTXO that we should spend
  submitTx u1 $ lockingTx dat sp val        -- User 1 submits "lockingTx" transaction

  waitUntil waitBeforeConsumingTx
  
  utxos <- utxoAt valScript                 -- Query blockchain to get all UTxOs at script
  let [(ref, out)] = utxos
  submitTx u2 $ consumingTx dat rdm u2 ref (txOutValue out)
  noErrors

