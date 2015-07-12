{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric   #-}
module Phb.Types (_ApiCustomer,Customer) where

import Control.Applicative ((<$>),(<*>))
import Control.Lens (Iso',iso,view,makeLensesFor,_Wrapped,from)
import GHC.Generics (Generic)
import Data.Text (Text)

import qualified Phb.Db as D
  
data Customer = Customer
  { customerId   :: Int
  , customerName :: Text
  } deriving (Generic)

makeLensesFor
  [ ("customerId","customerIdL")
  , ("customerName","customerNameL") ]
  ''Customer

_ApiCustomer :: Iso' Customer D.Customer
_ApiCustomer = iso apiCustomerToDb dbCustomerToApi

apiCustomerToDb :: Customer -> D.Customer
apiCustomerToDb = D.Customer
  <$> view (customerIdL.from _Wrapped)
  <*> view (customerNameL.from _Wrapped)

dbCustomerToApi :: D.Customer -> Customer
dbCustomerToApi = Customer
  <$> view (D.customerId._Wrapped)
  <*> view (D.customerName._Wrapped)
