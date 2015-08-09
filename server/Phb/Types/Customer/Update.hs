{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
module Phb.Types.Customer.Update
  ( CustomerUpdate(CustomerUpdate)
  , customerUpdateName
  , customerUpdateToDb
  ) where

import Control.Lens (makeLensesFor,_Wrapped,from,(^.))
import Data.Aeson   (FromJSON)
import GHC.Generics (Generic)
import Data.Text    (Text)

import qualified Phb.Db as D

data CustomerUpdate = CustomerUpdate
  { name :: Text
  } deriving (Generic)
makeLensesFor
  [ ("name","customerUpdateName") ]
  ''CustomerUpdate

customerUpdateToDb :: CustomerUpdate -> D.CustomerUpdate
customerUpdateToDb u = D.Customer
  { D._customerId   = ()
  , D._customerName = u^.customerUpdateName.from _Wrapped
  }

instance FromJSON CustomerUpdate
