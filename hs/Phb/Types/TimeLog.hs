{-# LANGUAGE TemplateHaskell #-}
module Phb.Types.TimeLog where

import BasePrelude ()
import Prelude     ()

import Control.Lens     hiding (Action)
import Database.Persist (Entity)

import Phb.Db.Internal
import Phb.Types.Task

data TimeLogWhole = TimeLogWhole
  { _timeLogWholeLog  :: Entity TimeLog
  , _timeLogWholeTask :: TaskWhole
  }
makeLenses ''TimeLogWhole
