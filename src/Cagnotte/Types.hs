{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Cagnotte.Types
  ( Wallet(..)
  , Transaction(..)
  , Amount(..)
  , Debts
  )
where

import Data.Map.Strict
import Numeric
import Data.Ratio

{-^
  A wallet defines either a particular person, or everyone
-}
data Wallet = Owner String {- Wallet owned by someone -}
            | Everybody    {- Common pot -}

newtype Amount = Amount Rational {- Amount defined in euros-}
               deriving (Num, Eq, Ord, Fractional)

{- Defines a transaction from a wallet to a wallet -}
data Transaction = Transaction
  { from   :: Wallet       -- ^ Issuer
  , to     :: Wallet       -- ^ Recipient
  , amount :: Amount       -- ^ Amount
  , label  :: Maybe String -- ^ a transaction may have a label
  }


{-^ We track debts by mapping a couple (debtor, loaner) to an amount -}
type Debts = Map (String, String) Amount

{- We instanciate the Show instance for pretty printing -}
instance Show Wallet where
  show (Owner name) = name
  show Everybody = "*"

instance Show Transaction where
  show t = (show $ from t)
         ++ " -> "
         ++ (show $ to t)
         ++ " : "
         ++ (show $ amount t)
         ++ case label t of
              Nothing -> ""
              Just s  -> " (" ++ s ++ ")"


instance Show Amount where
  show (Amount amount) = (showFFloat (Just 2) $ fromRat amount) "" ++ "€"
