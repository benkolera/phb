{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Phb.Api.Customer where

import Control.Applicative ((<$>),pure)
import Control.Lens ((^..),(^.),to,traversed,_Wrapped)
import Control.Monad (unless)
import Control.Monad.Error.Lens (throwing)

import Phb.Api.Internal
import Phb.Types.Customer
import Phb.Db (CustomerId(CustomerId))

import qualified Phb.Db as D
import qualified Phb.Types.RecordType as RT

import Phb.Types.Record  (RecordList(RecordList),Record(Record),record)

type CustomerList   = RecordList "customers"
type CustomerRecord = Record "customer"

requireCustomer
  :: CanApiFail e m
  => CustomerId
  -> Maybe D.Customer
  -> m D.Customer
requireCustomer (CustomerId i) = require RT.Customer (show i)

customerList :: CanApi c e m => m (CustomerList CustomerRead)
customerList =
  RecordList . (^..traversed.customerReadFromDb)
    <$> D.listCustomers

getCustomer
  :: CanApi c e m
  => D.CustomerId
  -> m (CustomerRecord CustomerRead)
getCustomer i = do
  c <- D.findCustomerById i >>= requireCustomer i
  pure $ Record (c^.customerReadFromDb)

putCustomer
  :: CanApi c e m
  => CustomerId
  -> CustomerRecord CustomerUpdate
  -> m (CustomerRecord CustomerRead)
putCustomer i cr = do
  let u = cr^.record.to customerUpdateToDb
  let n = u^.D.customerName
  cMay <- D.findCustomerByName n
  case cMay of
    Nothing -> pure ()
    Just c  -> unless (c^.D.customerId == i) $
      throwing _DuplicateRecord (RT.Customer,"Name",n^._Wrapped)
  D.updateCustomer i (cr^.record.to customerUpdateToDb)
  getCustomer i
