{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
module Phb.Db.Action where

import           BasePrelude
import           Prelude             ()

import qualified Control.Lens        as L
import           Control.Monad.Trans (MonadIO)
import           Data.Time           (UTCTime)
import           Database.Esqueleto

import           Phb.Db.Customer
import           Phb.Db.Esqueleto
import           Phb.Db.Internal
import           Phb.Db.Person
import qualified Phb.Types.Action    as T

loadAction :: (MonadIO m, Applicative m)
  => UTCTime
  -> Entity Action
  -> Db m T.Action
loadAction d (Entity aId a) = do
  cs <- loadActionCustomers aId
  ps <- loadActionPeople aId
  ns <- loadActionNotes aId d
  ss <- loadActionStatuses aId d
  return $ T.Action
    aId
    (a L.^.actionName)
    ps
    cs
    (a L.^. actionDue)
    ss
    ns
  where
    loadActionCustomers = loadRelatedCustomers ActionCustomerCustomer ActionCustomerAction
    loadActionPeople = loadRelatedPeople ActionPersonPerson ActionPersonAction
    loadActionNotes = loadTemporal ActionNoteAction ActionNoteStart ActionNoteFinish
    loadActionStatuses = loadTemporal ActionStatusAction ActionStatusStart ActionStatusFinish

loadActiveActions
  :: MonadIO m
  => UTCTime
  -> SqlPersistT m [Entity Action]
loadActiveActions = loadByStatus
  ActionId
  ActionStatusAction
  ActionStatusStart
  ActionStatusFinish
  (\ r _ -> [asc (r ^. ActionId)])
  (\ _ rs -> (rs ^. ActionStatusCompleted) ==. val True)
