{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE TemplateHaskell   #-}
module Phb.Db.Project where

import BasePrelude
import Prelude     ()

import           Control.Lens        hiding (from, (<.))
import           Control.Monad.Trans (MonadIO)
import           Data.Text           (Text)
import           Data.Time           (Day, UTCTime (..))
import           Database.Esqueleto  hiding ((^.))
import qualified Database.Esqueleto  as E
import qualified Database.Persist    as P

import           Phb.Db.Customer
import           Phb.Db.Enums
import           Phb.Db.Esqueleto
import           Phb.Db.Internal
import           Phb.Db.Person
import qualified Phb.Types.Project as T

data ProjectInput = ProjectInput
  { _projectInputName         :: Text
  , _projectInputStatusPhase  :: Text
  , _projectInputStatusColor  :: StatusColourEnum
  , _projectInputStatusDesc   :: Maybe Text
  , _projectInputStarted      :: Day
  , _projectInputFinished     :: Maybe Day
  , _projectInputPriority     :: Int
  , _projectInputNote         :: Text
  , _projectTargets           :: [T.TargetDate]
  , _projectInputCustomers    :: [Key Customer]
  , _projectInputStakeholders :: [Key Person]
  }
makeLenses ''ProjectInput

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
    (p^.projectPriority)
    (p^.projectStarted)
    (p^.projectFinished)
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
    orderBy [desc $ p E.^.ProjectPriority]
    return p

upsertProjectInput
  :: (MonadIO m,Applicative m)
  => ProjectInput
  -> UTCTime
  -> Maybe (Key Project)
  -> Db m (Key Project)
upsertProjectInput x cd kMay = do
  -- Should probably change this so that a DB error wouldn't just
  -- Crash us and should put a nice error in the form.
  case kMay of
   Nothing ->
     P.insert (newProject x) >>= updateSatellites
   Just k  ->
     P.replace k (newProject x) >> updateSatellites k
  where
    newProject (ProjectInput n _ _ _ s f p _ _ _ _) =
      Project n p s f
    updateSatellites k = do
      updateSatellite k (x ^. projectInputCustomers)
        ProjectCustomerProject
        ProjectCustomerId
        ProjectCustomerCustomer
        projectCustomerCustomer
        ProjectCustomer
      updateSatellite k (x ^. projectInputStakeholders)
        ProjectPersonProject
        ProjectPersonId
        ProjectPersonPerson
        projectPersonPerson
        ProjectPerson

      P.deleteWhere [ProjectTargetDateProject P.==. k]
      traverse_ (P.insert . inputTargetToDb k) (x ^. projectTargets)

      insertTemporal k
        ProjectStatusStart
        ProjectStatusFinish
        ProjectStatusId
        ProjectStatusProject
        fuzzyStatus
        cd $
          ProjectStatus k cd Nothing
            (x ^. projectInputStatusPhase)
            (x ^. projectInputStatusColor)
            (x ^. projectInputStatusDesc)

      insertTemporal k
        ProjectNoteStart
        ProjectNoteFinish
        ProjectNoteId
        ProjectNoteProject
        (view projectNoteNote)
        cd
        (ProjectNote k (x ^. projectInputNote) cd Nothing)

      return k

    fuzzyStatus = (,,)
      <$> view projectStatusPhase
      <*> view projectStatusColour
      <*> view projectStatusDesc

    inputTargetToDb k (T.TargetDate dy ds hw) =
      ProjectTargetDate k ds dy hw
