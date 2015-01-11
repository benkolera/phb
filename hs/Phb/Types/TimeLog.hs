{-# LANGUAGE TemplateHaskell #-}
module Phb.Types.TimeLog where

import BasePrelude
import Prelude     ()

import Control.Lens     hiding (Action)
import Data.Text        (Text)
import Database.Persist (Entity, Key)

import Phb.Db.Internal

data LinkKey
  = ProjectLink (Key Project)
  | EventLink (Key Event)
  | ActionLink (Key Action)
  | WorkCategoryLink (Key WorkCategory)
  | BacklogLink (Key Backlog)
  deriving (Show,Eq,Ord)

data TimeLogLink = TimeLogLink
  { _timeLogLinkName :: Text
  , _timeLogLinkKey  :: LinkKey
  }
makeLenses ''TimeLogLink

data TimeLogWhole = TimeLogWhole
  { _timeLogWholeLog    :: Entity TimeLog
  , _timeLogWholePerson :: Entity Person
  , _timeLogWholeLink   :: Maybe TimeLogLink
  }
makeLenses ''TimeLogWhole
