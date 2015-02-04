{-# LANGUAGE TemplateHaskell #-}
module Phb.Types.Standup where

import           Control.Lens
import           Database.Persist      (Entity)

import qualified Phb.Db.Internal       as D
import           Phb.Types.Task
import           Phb.Types.TimeSummary

data StandupPersonSummary = StandupPersonSummary
  { _standupPerson         :: Entity D.Person
  , _standupYesterdayTasks :: [(TaskWhole,TimeSummary,Bool)]
  , _standupTodayTasks     :: [TaskWhole]
  }
makeLenses ''StandupPersonSummary
