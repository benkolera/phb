{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Phb.Types.Task where

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
makePrisms ''LinkKey

data TaskLink = TaskLink
  { _taskLinkName :: Text
  , _taskLinkKey  :: LinkKey
  }
makeLenses ''TaskLink

data TaskWhole = TaskWhole
  { _taskWholeTask   :: Entity Task
  , _taskWholePerson :: Entity Person
  , _taskWholeLink   :: Maybe TaskLink
  }
makeLenses ''TaskWhole

linkTypeName :: LinkKey -> Text
linkTypeName (ProjectLink _)      = "Project"
linkTypeName (BacklogLink _)      = "Backlog"
linkTypeName (ActionLink _)       = "Action"
linkTypeName (EventLink _)        = "Event"
linkTypeName (WorkCategoryLink _) = "Work Category"
