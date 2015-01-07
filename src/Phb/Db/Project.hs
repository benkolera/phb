{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
module Phb.Db.Project where

import BasePrelude
import Prelude     ()

import           Control.Lens        hiding (from, (<.))
import           Control.Monad.Trans (MonadIO)
import           Data.Text           (Text)
import           Data.Time           (UTCTime (..))
import           Database.Esqueleto  hiding ((^.))
import qualified Database.Persist    as P

import           Phb.Db.Customer
import           Phb.Db.Enums
import           Phb.Db.Esqueleto
import           Phb.Db.Internal
import           Phb.Db.Person
import qualified Phb.Types.Project as T

projectStatusColourHuman :: StatusColourEnum -> Text
projectStatusColourHuman StatusGreen = "Green"
projectStatusColourHuman StatusAmber = "Amber"
projectStatusColourHuman StatusRed   = "Red"

loadProject :: (MonadIO m, Applicative m)
  => UTCTime
  -> Entity Project
  -> Db m T.Project
loadProject d (Entity pId p) = do
  tws  <- P.selectList
          [ TimeLogProject P.==. Just pId
          , TimeLogDay     P.<=. utctDay d
          ] []
  ptds <- P.selectList [ProjectTargetDateProject P.==. pId]      []
  cs   <- loadProjectCustomers pId
  ps   <- loadProjectPeople pId
  ss   <- loadProjectStatuses pId d
  ns   <- loadProjectNotes pId d
  return $ T.Project
    pId
    (p ^. projectName)
    cs
    (projectStatus <$> ss)
    ps
    (fmap (targetDate . entityVal) ptds)
    (calcDaysEffortSpent tws)
    (p ^. projectStarted)
    (p ^. projectFinished)
    ns
  where
    loadProjectPeople = loadRelatedPeople ProjectPersonPerson ProjectPersonProject
    loadProjectCustomers = loadRelatedCustomers ProjectCustomerCustomer ProjectCustomerProject
    loadProjectNotes = loadTemporal ProjectNoteProject ProjectNoteStart ProjectNoteFinish
    loadProjectStatuses = loadTemporal ProjectStatusProject ProjectStatusStart ProjectStatusFinish
    projectStatus (Entity k ps) = T.ProjectStatus
     k
     (ps ^. projectStatusPhase)
     (ps ^. projectStatusColour)
     (ps ^. projectStatusDesc)

    targetDate (ProjectTargetDate _ dsc day handwavy) =
      T.TargetDate day dsc handwavy
    calcDaysEffortSpent tws =
      (fromIntegral (sum (fmap (view timeLogMinutes . entityVal) tws)) / 60 / 8)

loadActiveProjects :: MonadIO m => UTCTime -> SqlPersistT m [Entity Project]
loadActiveProjects ct =
  select $ from $ \ p -> do
    where_ (withinBounds p ProjectStarted ProjectFinished (utctDay ct))
    return p
