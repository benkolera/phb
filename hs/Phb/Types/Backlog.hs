{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Phb.Types.Backlog where

import BasePrelude hiding (index)
import Prelude     ()

import Control.Lens
import Data.Text        (Text)
import Database.Persist (Entity, Key)

import qualified Phb.Db.Internal as D

data Backlog = Backlog
  { _backlogKey          :: Key D.Backlog
  , _backlogName         :: Text
  , _backlogPriority     :: Int
  , _backlogCustomers    :: [Entity D.Customer]
  , _backlogStatuses     :: [Entity D.BacklogStatus]
  , _backlogStakeholders :: [Entity D.Person]
  , _backlogNotes        :: [Entity D.BacklogNote]
  } deriving Show
makeLenses ''Backlog

backlogStatusLatest :: IndexedTraversal' Int Backlog (Entity D.BacklogStatus)
backlogStatusLatest = backlogStatuses . traversed . index 0

backlogNoteLatest :: IndexedTraversal' Int Backlog (Entity D.BacklogNote)
backlogNoteLatest = backlogNotes . traversed . index 0
