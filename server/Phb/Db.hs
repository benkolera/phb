module Phb.Db
  ( module Phb.Db.Customer 
  , module Phb.Db.Internal
  ) where

import Phb.Db.Internal (DbEnv(DbEnv),DbError,HasDbEnv(dbEnv),AsDbError(_DbError))
import Phb.Db.Customer
  ( Customer
  , CustomerId(CustomerId)
  , Customer'(..)
  , CustomerUpdate
  , CustomerCreate
  , listCustomers
  , customerId
  , customerName
  , updateCustomer
  , findCustomerById
  , findCustomerByName
  )
