{-# LANGUAGE Arrows                     #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Phb.Db.Customer where

import Control.Arrow                        (returnA)
import Control.Lens                         hiding ((.>))
import Control.Monad.Trans                  (liftIO)
import Data.Profunctor.Product.TH           (makeAdaptorAndInstance)
import Data.Text                            (Text)
import Data.Typeable                        (Typeable)
import Database.PostgreSQL.Simple.FromField (FromField (..))
import Opaleye
import Opaleye.Internal.RunQuery            (QueryRunnerColumnDefault (..))

import Phb.Db.Internal

data Customer' a b = Customer
  { _customerId   :: a
  , _customerName :: b
  } deriving (Eq,Show)
makeLenses ''Customer'

newtype CustomerId = CustomerId Int deriving (Eq,Typeable,Read,Num)
makeWrapped ''CustomerId

newtype CustomerName = CustomerName Text deriving (Eq,Typeable,Read)
makeWrapped ''CustomerName

type Customer = Customer' CustomerId CustomerName

type CustomerColumn = Customer' (Column PGInt4) (Column PGText)

makeAdaptorAndInstance "pCustomer" ''Customer'

type NewCustomer = Customer' (Maybe CustomerId) CustomerName

type NewCustomerColumn = Customer' (Maybe (Column PGInt4)) (Column PGText)

customerTable :: Table NewCustomerColumn CustomerColumn
customerTable = Table "customer" $ pCustomer Customer
  { _customerId   = optional "id"
  , _customerName = required "name"
  }

customerQuery :: Query CustomerColumn
customerQuery = queryTable customerTable

listCustomers :: CanDb m c e => m [Customer]
listCustomers = liftQuery $ customerQuery

insertCustomer :: CanDb m c e => NewCustomer -> m [Int]
insertCustomer = liftInsertReturning customerTable (view customerId) . packNew

toWrite :: (NewCustomerColumn -> NewCustomerColumn) -> CustomerColumn -> NewCustomerColumn
toWrite f t = t & customerId .~ Nothing & f

findCustomerById :: CanDb m c e => CustomerId -> m (Maybe Customer)
findCustomerById i = liftQueryFirst $ proc () -> do
  t <- customerQuery -< ()
  restrict -< t^.customerId .== packCustomerId i
  returnA  -< t

packNew :: NewCustomer -> NewCustomerColumn
packNew = pCustomer Customer
  { _customerId        = fmap packCustomerId
  , _customerName      = packCustomerName
  }

packCustomerId :: CustomerId -> Column PGInt4
packCustomerId = (^._Wrapped.to pgInt4)

packCustomerName :: CustomerName -> Column PGText
packCustomerName = (^._Wrapped.to pgStrictText)

instance QueryRunnerColumnDefault PGInt4 CustomerId where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance FromField CustomerId where
  fromField = derivePGField CustomerId

instance QueryRunnerColumnDefault PGText CustomerName where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance FromField CustomerName where
  fromField = derivePGField CustomerName
