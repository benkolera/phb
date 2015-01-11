{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Phb.Types.Heartbeat where

import           BasePrelude
import           Prelude           ()

import           Control.Lens      (makeLenses)
import           Data.Text         (Text)
import           Data.Time         (Day)
import           Database.Persist  (Entity, Key)

import qualified Phb.Db.Internal   as D
import           Phb.Types.Action
import           Phb.Types.Backlog
import           Phb.Types.Event
import           Phb.Types.Project
import           Phb.Types.Success

data HeartbeatSupport = HeartbeatSupport
  { _heartbeatSupportLabel  :: Text
  , _heartbeatSupportHours  :: Double
  , _heartbeatSupportPeople :: [Entity D.Person]
  }
makeLenses ''HeartbeatSupport

data Heartbeat = Heartbeat
  { _heartbeatKey        :: Key D.Heartbeat
  , _heartbeatStart      :: Day
  , _heartbeatFinish     :: Day
  , _heartbeatUpcoming   :: [Text]
  , _heartbeatHighlights :: [Text]
  , _heartbeatSuccesses  :: [Success]
  , _heartbeatProjects   :: [Project]
  , _heartbeatBacklog    :: [Backlog]
  , _heartbeatEvents     :: [Event]
  , _heartbeatActions    :: [Action]
  , _heartbeatSupport    :: [HeartbeatSupport]
  }
makeLenses ''Heartbeat
