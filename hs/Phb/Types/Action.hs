{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Phb.Types.Action where

import           BasePrelude      hiding (index)
import           Prelude          ()

import           Control.Lens     (IndexedTraversal', index, makeLenses,
                                   traversed)
import           Data.Text        (Text)
import           Data.Time
import           Database.Persist (Entity, Key)

import qualified Phb.Db.Internal  as D

data Action = Action
  { _actionKey       :: Key D.Action
  , _actionName      :: Text
  , _actionPeople    :: [Entity D.Person]
  , _actionCustomers :: [Entity D.Customer]
  , _actionDue       :: Day
  , _actionStatuses  :: [Entity D.ActionStatus]
  , _actionNotes     :: [Entity D.ActionNote]
  } deriving Show
makeLenses ''Action

actionStatusLatest :: IndexedTraversal' Int Action (Entity D.ActionStatus)
actionStatusLatest = actionStatuses . traversed . index 0

actionNoteLatest :: IndexedTraversal' Int Action (Entity D.ActionNote)
actionNoteLatest = actionNotes . traversed . index 0
