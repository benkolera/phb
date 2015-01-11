{-# LANGUAGE TypeFamilies #-}
module Phb.Db.Customer where

import           Control.Monad.Trans (MonadIO)
import           Database.Esqueleto
import qualified Database.Persist    as P

import Phb.Db.Internal

loadRelatedCustomers
  :: ( PersistEntity r
    , PersistField i
    , MonadIO m
    , PersistEntityBackend r ~ SqlBackend
    )
  => EntityField r (Key Customer)
  -> EntityField r i
  -> i
  -> SqlPersistT m [Entity Customer]
loadRelatedCustomers custFkCol relIdCol relId =
  select $ from $ \(bc `InnerJoin` c) -> do
    on (bc ^. custFkCol ==. c ^. CustomerId)
    where_ (val relId ==. bc ^. relIdCol)
    return c

loadActiveCustomers :: (MonadIO m) => SqlPersistT m [Entity Customer]
loadActiveCustomers = P.selectList [] [P.Asc CustomerId]
