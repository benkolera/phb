{-# LANGUAGE TypeFamilies #-}
module Phb.Db.Person where

import           BasePrelude         hiding (on)
import           Prelude             ()

import           Control.Monad.Trans (MonadIO)
import           Data.Time           (Day)
import           Database.Esqueleto

import           Phb.Db.Internal

loadRelatedPeople
  :: ( PersistEntity r
    , PersistField i
    , MonadIO m
    , PersistEntityBackend r ~ SqlBackend
    )
  => EntityField r (Key Person)
  -> EntityField r i
  -> i
  -> SqlPersistT m [Entity Person]
loadRelatedPeople custFkCol relIdCol relId =
  select $ from $ \(bc `InnerJoin` c) -> do
    on (bc ^. custFkCol ==. c ^. PersonId)
    where_ (val relId ==. bc ^. relIdCol)
    return c

missingTimeLogsFor
  :: (MonadIO m)
  => Day
  -> SqlPersistT m [Entity Person]
missingTimeLogsFor cd = do
  selectDistinct $ from $ \ p -> do
    where_ (
      p ^. PersonLogsTime ==. val True
      &&. (p ^. PersonId `notIn` (subList_select timeLogsForToday))
      )
    return p
  where
    timeLogsForToday = from $ \ tl -> do
      where_ ( tl ^. TimeLogDay ==. val cd )
      return (tl^.TimeLogPerson)
