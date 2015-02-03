{-# LANGUAGE TemplateHaskell #-}
module Phb.Types.Standup where

import Control.Lens
import Database.Persist (Entity)

import qualified Phb.Db                as D
import           Phb.Types.Task
import           Phb.Types.TimeSummary

data StandupPersonSummary = StandupPersonSummary
  { _person         :: Entity D.Person
  , _yesterdayTasks :: [(TaskWhole,TimeSummary,Bool)]
  , _todayTasks     :: [TaskWhole]
  }
makeLenses ''StandupPersonSummary
