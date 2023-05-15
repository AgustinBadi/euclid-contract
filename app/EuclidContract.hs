{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell     #-}

module EuclidContract where

import qualified Plutus.V2.Ledger.Api as PlutusV2
import           PlutusTx             (BuiltinData, compile)
import           PlutusTx.Prelude     (Bool, Eq ((==)), Ord ((>)), Integer, quotient, modulo, (*), (-), otherwise,
                                      traceIfFalse, ($), (&&))
import           Prelude              (IO)
import           Utilities            (wrapValidator, writeValidatorToFile, writeDataToFile)


instance (Eq a, Eq b, Eq c) => Eq (a, b, c) where
    {-# INLINABLE (==) #-}
    (a, b, c) == (a', b', c') = a == a' && b == b' && c == c'

{-# INLINABLE gdc #-}
gdc :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> (Integer, Integer, Integer)
gdc a b s1 t1 s2 t2
 | resto == 0 = (b,s2,t2)
 | otherwise = gdc b resto s2 t2 s3 t3 
 where
    resto = modulo a b 
    resultado = quotient a b
    s3 = (s1 - resultado * s2)
    t3 = (t1 - resultado * t2)

{-# INLINABLE mkEuclidContract #-}
--                  Datum                 Redeemer                       ScriptContext
mkEuclidContract :: (Integer, Integer) -> (Integer, Integer, Integer) -> PlutusV2.ScriptContext -> Bool
mkEuclidContract (a,b) (r,s,t) _ = 
    traceIfFalse "Wrong paramters: 'a' must be greater than 'b'." (a > b) &&
    traceIfFalse "Wrong parameters: 'a' and 'b' have to be greater than 0." (a > 0 && b > 0) &&
    traceIfFalse "Your bet is wrong! :(" isGdc
  where 
    isGdc :: Bool  
    isGdc = gdc a b 1 0 0 1 == (r,s,t)

{-# INLINABLE  mkWrappedValidator #-}
mkWrappedValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedValidator = wrapValidator mkEuclidContract

validator :: PlutusV2.Validator
validator = PlutusV2.mkValidatorScript $$(PlutusTx.compile [|| mkWrappedValidator ||])

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

main :: IO ()
main = writeValidatorToFile "./contractdata/euclid.plutus" validator

writeDatum :: (Integer, Integer) -> IO ()
writeDatum = writeDataToFile "./contractdata/gdcValues.json" 

writeRedeemer :: (Integer, Integer, Integer) -> IO ()
writeRedeemer = writeDataToFile "./contractdata/bet.json" 







