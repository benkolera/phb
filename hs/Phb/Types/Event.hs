{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Phb.Types.Event where

import BasePrelude hiding (index)
import Prelude     ()

import Control.Lens
import Data.Text        (Text)
import Data.Time        (LocalTime)
import Database.Persist (Entity, Key)

import qualified Phb.Db.Internal as D

data Event = Event
  { _eventKey       :: Key D.Event
  , _eventName      :: Text
  , _eventCustomers :: [Entity D.Customer]
  , _eventDesc      :: [Text]
  , _eventImpact    :: [Text]
  , _eventDuration  :: (LocalTime,Maybe LocalTime)
  , _eventStatuses  :: [Entity D.EventStatus]
  , _eventNotes     :: [Entity D.EventNote]
  } deriving Show
makeLenses ''Event

eventStatusLatest :: IndexedTraversal' Int Event (Entity D.EventStatus)
eventStatusLatest = eventStatuses . traversed . index 0

eventNoteLatest :: IndexedTraversal' Int Event (Entity D.EventNote)
eventNoteLatest = eventNotes . traversed . index 0
