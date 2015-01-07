{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
module Phb.Db.Event where

import BasePrelude
import Prelude     ()

import qualified Control.Lens        as L
import           Control.Monad.Trans (MonadIO, liftIO)
import           Data.Bifunctor      (bimap)
import qualified Data.Text           as T
import           Data.Time           (UTCTime, getCurrentTimeZone,
                                      utcToLocalTime)
import           Database.Esqueleto

import           Phb.Db.Customer
import           Phb.Db.Enums
import           Phb.Db.Esqueleto
import           Phb.Db.Internal
import qualified Phb.Types.Event  as T

eventStatusHuman :: EventStatusEnum -> T.Text
eventStatusHuman EventSlaMet     = "Yes"
eventStatusHuman EventSlaJustMet = "Barely"
eventStatusHuman EventSlaNotMet  = "No"

-- activeEventFilter :: UTCTime -> Filter Event
-- activeEventFilter = (EventFinish <.)

loadEvent :: (MonadIO m, Applicative m)
  => UTCTime
  -> Entity Event
  -> Db m T.Event
loadEvent d (Entity eId e) = do
  cs <- loadEventCustomers eId
  ns <- loadEventNotes eId d
  ss <- loadEventStatuses eId d
  tz  <- liftIO $ getCurrentTimeZone
  let toLocal = utcToLocalTime tz
  return $ T.Event
    eId
    (e L.^. eventName)
    cs
    (T.lines . L.view eventDesc $ e)
    (T.lines . L.view eventImpact $ e)
    (bimap toLocal (fmap toLocal) . (L.view eventStart &&& L.view eventFinish) $ e)
    ss
    ns
  where
    loadEventCustomers = loadRelatedCustomers EventCustomerCustomer EventCustomerEvent
    loadEventNotes = loadTemporal EventNoteEvent EventNoteStart EventNoteFinish
    loadEventStatuses = loadTemporal EventStatusEvent EventStatusStart EventStatusFinish

loadActiveEvents
  :: MonadIO m
  => UTCTime
  -> SqlPersistT m [Entity Event]
loadActiveEvents ct =
  select $ from $ \ e -> do
    where_ (withinBounds e EventStart EventFinish ct)
    return e
