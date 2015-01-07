{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
module Phb.Db.Backlog where

import BasePrelude hiding (on)
import Prelude     ()

import qualified Control.Lens        as L
import           Control.Monad.Trans (MonadIO)
import           Data.Text           (Text)
import           Data.Time           (UTCTime)
import           Database.Esqueleto

import           Phb.Db.Enums
import           Phb.Db.Esqueleto
import           Phb.Db.Internal
import qualified Phb.Types.Backlog as T

backlogStatusHuman :: BacklogStatusEnum -> Text
backlogStatusHuman BacklogScoped = "Scoped"
backlogStatusHuman BacklogNeedsScoping = "Needs Scoping"
backlogStatusHuman BacklogInCommercials   = "In Commercial Discussions"
backlogStatusHuman BacklogProjectStarted = "Project Started"
backlogStatusHuman BacklogRejected = "Rejected"

loadBacklog :: (MonadIO m, Applicative m)
  => UTCTime
  -> Entity Backlog
  -> Db m T.Backlog
loadBacklog d (Entity bId b) = do
  cs <- select $ from $ \(bc `InnerJoin` c) -> do
    on (bc ^. BacklogCustomerCustomer ==. c ^. CustomerId)
    where_ (val bId ==. bc ^. BacklogCustomerBacklog)
    return c
  ps <- select $ from $ \(bp `InnerJoin` p) -> do
    on (bp ^. BacklogPersonPerson ==. p ^. PersonId)
    where_ (val bId ==. bp ^. BacklogPersonBacklog)
    return p
  ns <- loadBacklogNotes bId d
  ss <- loadBacklogStatuses bId d
  return $ T.Backlog
    bId
    (b L.^. backlogName)
    cs
    ss
    ps
    ns
  where
    loadBacklogNotes = loadTemporal BacklogNoteBacklog BacklogNoteStart BacklogNoteFinish
    loadBacklogStatuses = loadTemporal BacklogStatusBacklog BacklogStatusStart BacklogStatusFinish

loadActiveBacklog
  :: MonadIO m
  => UTCTime
  -> SqlPersistT m [Entity Backlog]
loadActiveBacklog = loadByStatus
  BacklogId
  BacklogStatusBacklog
  BacklogStatusStart
  BacklogStatusFinish
  (\ r _ -> [asc (r ^. BacklogId)])
  (\ _ rs -> (rs ^. BacklogStatusStatus) `in_` (valList activeBacklogStatuses))

activeBacklogStatuses :: [BacklogStatusEnum]
activeBacklogStatuses =
  [ BacklogScoped
  , BacklogNeedsScoping
  , BacklogInCommercials
  ]
