{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Phb.Types.Project where

import BasePrelude hiding (index)
import Prelude     ()

import Control.Lens
import Data.Text        (Text)
import Data.Time
import Database.Persist (Entity, Key)

import           Phb.Db.Enums
import qualified Phb.Db.Internal as D

data ProjectStatus = ProjectStatus
  { _projectStatusKey    :: Key D.ProjectStatus
  , _projectStatusPhase  :: Text
  , _projectStatusColour :: StatusColourEnum
  , _projectStatusDesc   :: Maybe Text
  } deriving Show
makeLenses ''ProjectStatus

data TargetDate = TargetDate
  { _targetDateDay      :: Day
  , _targetDateDesc     :: Text
  , _targetDateHandwavy :: Maybe Text
  } deriving Show
makeLenses ''TargetDate

data Project = Project
  { _projectKey          :: Key D.Project
  , _projectName         :: Text
  , _projectCustomers    :: [Entity D.Customer]
  , _projectStatuses     :: [ProjectStatus]
  , _projectStakeholders :: [Entity D.Person]
  , _projectTargetDates  :: [TargetDate]
  , _projectEffortDays   :: Double
  , _projectStarted      :: Day
  , _projectFinished     :: Maybe Day
  , _projectNotes        :: [Entity D.ProjectNote]
  } deriving Show
makeLenses ''Project

projectStatusLatest :: IndexedTraversal' Int Project ProjectStatus
projectStatusLatest = projectStatuses . traversed . index 0

projectNoteLatest :: IndexedTraversal' Int Project (Entity D.ProjectNote)
projectNoteLatest = projectNotes . traversed . index 0
