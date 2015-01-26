{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Phb.Db.Esqueleto where

import BasePrelude hiding (insert,on,isNothing)
import Prelude ()

import Control.Error (headMay)
import Control.Monad.Trans (MonadIO)
import Data.Time           (UTCTime (..))
import Database.Esqueleto
import Database.Esqueleto.Internal.Sql (SqlSelect)

loadTemporal
  :: ( MonadIO m
    , PersistEntity a
    , PersistField k
    , PersistEntityBackend a ~ SqlBackend
    )
  => EntityField a k
  -> EntityField a UTCTime
  -> EntityField a (Maybe UTCTime)
  -> k
  -> UTCTime
  -> SqlPersistT m [Entity a]
loadTemporal pKeyCol sCol fCol pId ct =
  select $ from $ \(xs) -> do
    where_ (val pId ==. xs ^. pKeyCol &&. (withinBounds xs sCol fCol ct))
    orderBy [desc $ xs ^. sCol]
    return xs

insertTemporal
  :: ( PersistEntity val
    , PersistField fk
    , MonadIO m
    , Functor m
    , Eq a
    , PersistEntityBackend val ~ SqlBackend
    )
  => fk
  -> EntityField val UTCTime
  -> EntityField val (Maybe UTCTime)
  -> EntityField val (Key val)
  -> EntityField val fk
  -> (val -> a)
  -> UTCTime
  -> val
  -> SqlPersistT m ()
insertTemporal k refStart refFinish refKey refFKey extractFuzz ct newRef = do
  refMay <- selectFirstEsq $ from $ \ rs -> do
    where_ (rs ^. refFKey ==. val k &&. withinBounds rs refStart refFinish ct)
    return rs
  case refMay of
    Nothing -> void $ insert newRef
    Just r  -> unless (fuzzyEq newRef (entityVal r)) $ do
      void $ insert newRef
      update $ \ nr -> do
        set nr [refFinish =. val (Just ct)]
        where_ (nr ^. refKey ==. val (entityKey r))
  where
    fuzzyEq a b = extractFuzz a == extractFuzz b

selectFirstEsq
  :: ( SqlSelect a1 a
    , MonadIO m
    , Functor m
    )
  => SqlQuery a1
  -> SqlPersistT m (Maybe a)
selectFirstEsq = fmap headMay . select

loadByStatus
  :: ( PersistEntity r
    , PersistEntity rs
    , PersistField k
    , MonadIO m
    , PersistEntityBackend r ~ SqlBackend
    , PersistEntityBackend rs ~ SqlBackend
    )
  => EntityField r k
  -> EntityField rs k
  -> EntityField rs UTCTime
  -> EntityField rs (Maybe UTCTime)
  -> (SqlExpr (Entity r) -> SqlExpr (Entity rs) -> [SqlExpr OrderBy])
  -> (SqlExpr (Entity r) -> SqlExpr (Entity rs) -> SqlExpr (Value Bool))
  -> UTCTime
  -> SqlPersistT m [Entity r]
loadByStatus pkCol fkCol sCol fCol o w ct = do
  selectDistinct $ from $ \(r `InnerJoin` rs) -> do
    on (r ^. pkCol ==. rs ^. fkCol)
    where_ ((w r rs) &&. (withinBounds rs sCol fCol ct))
    orderBy (o r rs)
    return r

withinPeriod
  :: ( Esqueleto query expr backend
    , PersistEntity val
    , PersistField typ
    )
  => expr (Entity val)
  -> EntityField val typ
  -> typ
  -> typ
  -> expr (Value Bool)
withinPeriod r dc sd fd =
  ((r ^. dc) >=. (val sd)) &&. (r ^. dc) <=. (val $ fd)

withinBounds
  :: ( Esqueleto query expr backend
    , PersistEntity val
    , PersistField t )
  => expr (Entity val)
  -> EntityField val t
  -> EntityField val (Maybe t)
  -> t
  -> expr (Value Bool)
withinBounds r sc fc ct =
  ((r ^. sc) <=. (val ct))
  &&. ((isNothing $ r ^. fc)
       ||. (r ^. fc) >=. (val $ Just ct))
  -- Being inclusive of the start and end is going to be a terabad mistake
  -- but it is necessary given some things are Day columns and can thus
  -- start and end on the same instant.
