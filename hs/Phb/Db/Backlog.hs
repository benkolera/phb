{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE TemplateHaskell   #-}
module Phb.Db.Backlog where

import BasePrelude hiding (insert, on)
import Prelude     ()

import           Control.Lens        hiding (from, (^.))
import qualified Control.Lens        as L
import           Control.Monad.Trans (MonadIO, liftIO)
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Time           (UTCTime)
import           Database.Esqueleto
import qualified Database.Persist    as P

import           Phb.Db.Enums
import           Phb.Db.Esqueleto
import           Phb.Db.Internal
import           Phb.Db.Project
import qualified Phb.Types.Backlog as T
import           Phb.Util

data BacklogInput = BacklogInput
  { _backlogInputName         :: Text
  , _backlogInputStatus       :: BacklogStatusEnum
  , _backlogInputPriority     :: Int
  , _backlogInputNote         :: Text
  , _backlogInputCustomers    :: [Key Customer]
  , _backlogInputStakeholders :: [Key Person]
  }
makeLenses ''BacklogInput

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
    (b L.^. backlogPriority)
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
  (\ r _ -> [desc (r ^. BacklogPriority)])
  (\ _ rs -> (rs ^. BacklogStatusStatus) `in_` (valList activeBacklogStatuses))

loadActiveBacklogForPeriod
  :: MonadIO m
  => UTCTime
  -> UTCTime
  -> SqlPersistT m [Entity Backlog]
loadActiveBacklogForPeriod = loadByStatusPeriod
  BacklogId
  BacklogStatusBacklog
  BacklogStatusStart
  BacklogStatusFinish
  (\ r _ -> [desc (r ^. BacklogPriority)])
  (\ _ rs -> (rs ^. BacklogStatusStatus) `in_` (valList activeBacklogStatuses))

activeBacklogStatuses :: [BacklogStatusEnum]
activeBacklogStatuses =
  [ BacklogScoped
  , BacklogNeedsScoping
  , BacklogInCommercials
  ]

upsertBacklog
  :: (MonadIO m, Applicative m)
  => BacklogInput
  -> UTCTime
  -> Maybe (Key Backlog)
  -> Db m (Key Backlog)
upsertBacklog x cd kMay = do
  case kMay of
   Nothing -> P.insert (newBacklog x) >>= updateSatellites
   Just k  -> P.replace k (newBacklog x) >> updateSatellites k

  where
    newBacklog (BacklogInput n _ p _ _ _) = Backlog n p
    updateSatellites k = do
      updateSatellite k (x L.^.backlogInputCustomers)
        BacklogCustomerBacklog
        BacklogCustomerId
        BacklogCustomerCustomer
        backlogCustomerCustomer
        BacklogCustomer
      updateSatellite k (x L.^.backlogInputStakeholders)
        BacklogPersonBacklog
        BacklogPersonId
        BacklogPersonPerson
        backlogPersonPerson
        BacklogPerson

      updateStatus k cd (view backlogInputStatus x)

      insertTemporal k
        BacklogNoteStart
        BacklogNoteFinish
        BacklogNoteId
        BacklogNoteBacklog
        (view backlogNoteNote)
        cd
        (BacklogNote k (x L.^.backlogInputNote) cd Nothing)

      return k

updateStatus
  :: (MonadIO m, Functor m)
  => Key Backlog
  -> UTCTime
  -> BacklogStatusEnum
  -> SqlPersistT m ()
updateStatus k cd s =
  insertTemporal k
    BacklogStatusStart
    BacklogStatusFinish
    BacklogStatusId
    BacklogStatusBacklog
    (view backlogStatusStatus)
    cd
    (BacklogStatus k cd Nothing s)

promoteBacklog
  :: (MonadIO m,Applicative m)
  => UTCTime
  -> Entity Backlog
  -> Db m (Key Project)
promoteBacklog ct e = do
  b <- loadBacklog ct e
  cd <- liftIO $ localDayFromUTC ct
  newP <- upsertProjectInput (mkPrjInput b cd) ct Nothing
  updateStatus (entityKey e) ct BacklogProjectStarted
  pure newP
  where
    mkPrjInput i cd = ProjectInput
      (i L.^.T.backlogName)
      "Development"
      StatusGreen
      Nothing
      cd
      Nothing
      (i L.^.T.backlogPriority)
      (T.unlines (i L.^..T.backlogNotes.traverse.eVal.backlogNoteNote))
      []
      (i L.^..T.backlogCustomers.traverse.eKey)
      (i L.^..T.backlogStakeholders.traverse.eKey)
