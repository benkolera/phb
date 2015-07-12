{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module Phb.Types.Customer 
  ( _ApiCustomer
  , Customer(Customer)
  , customerId
  , customerName
  ) where

import Control.Applicative ((<$>),(<*>))
import Control.Lens (Iso',iso,view,makeLensesFor,_Wrapped,from)
import GHC.Generics (Generic)
import Data.Text (Text)

import qualified Phb.Db as D

data Customer = Customer
  { id   :: Int
  , name :: Text
  } deriving (Generic)
makeLensesFor
  [ ("id","customerId")
  , ("name","customerName") ]
  ''Customer

_ApiCustomer :: Iso' Customer D.Customer
_ApiCustomer = iso apiCustomerToDb dbCustomerToApi

apiCustomerToDb :: Customer -> D.Customer
apiCustomerToDb = D.Customer
  <$> view (customerId.from _Wrapped)
  <*> view (customerName.from _Wrapped)

dbCustomerToApi :: D.Customer -> Customer
dbCustomerToApi = Customer
  <$> view (D.customerId._Wrapped)
  <*> view (D.customerName._Wrapped)
