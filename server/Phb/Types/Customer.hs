{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-orphans      #-}
module Phb.Types.Customer 
  ( CustomerRead(CustomerRead)
  , CustomerUpdate(CustomerUpdate)
  , CustomerCreate(CustomerCreate)
  , customerReadFromDb
  , customerUpdateToDb
  ) where

import Control.Error  (hush)
import Data.Text.Read (decimal)
import Servant        (FromText(fromText))

import Phb.Types.Customer.Create
import Phb.Types.Customer.Update
import Phb.Types.Customer.Read
import Phb.Db                    (CustomerId(CustomerId))

instance FromText CustomerId where
  fromText = fmap (CustomerId . fst) . hush . decimal 
