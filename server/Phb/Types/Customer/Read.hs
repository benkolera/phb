{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
module Phb.Types.Customer.Read
  ( customerReadFromDb
  , CustomerRead(CustomerRead)
  , customerId
  , customerName
  ) where

import Control.Applicative ((<$>),(<*>))
import Control.Lens        (Getter,view,makeLensesFor,_Wrapped,to)
import Data.Aeson          (ToJSON)
import GHC.Generics        (Generic)
import Data.Text           (Text)

import qualified Phb.Db as D

data CustomerRead = CustomerRead
  { id   :: Int
  , name :: Text
  } deriving (Generic)
makeLensesFor
  [ ("id","customerId")
  , ("name","customerName") ]
  ''CustomerRead

customerReadFromDb :: Getter D.Customer CustomerRead
customerReadFromDb = to $ CustomerRead
  <$> view (D.customerId._Wrapped)
  <*> view (D.customerName._Wrapped)

instance ToJSON CustomerRead
