{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Site.Event where

import BasePrelude hiding (index, insert)
import Prelude     ()

import           Control.Lens
import           Control.Monad.Trans           (lift, liftIO)
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Data.Time                     (LocalTime, getCurrentTime,
                                                getCurrentTimeZone,
                                                localTimeToUTC)
import           Database.Persist.Sql
import           Heist
import qualified Heist.Compiled                as C
import qualified Heist.Compiled.LowLevel       as C
import           Snap                          (ifTop, redirect)
import           Snap.Snaplet.Heist.Compiled
import           Snap.Snaplet.Persistent       (runPersist)
import           Text.Digestive
import           Text.Digestive.Heist.Compiled
import           Text.Digestive.Snap

import           Phb.Db
import qualified Phb.Types.Event as T
import           Phb.Util
import           Site.Internal

eventRoutes :: PhbRoutes
eventRoutes =
  [("/events"       ,ifTop . userOrIndex . render $ "events/all")
  ,("/events/create",ifTop . userOrIndex . render $ "events/create")
  ,("/events/:id/edit",ifTop . userOrIndex . render $ "events/edit")
  ]

data EventInput = EventInput
  { _eventInputName      :: Text
  , _eventInputDesc      :: Text
  , _eventInputImpact    :: Text
  , _eventInputStart     :: LocalTime
  , _eventInputFinish    :: Maybe LocalTime
  , _eventInputStatus    :: EventStatusEnum
  , _eventInputNote      :: Text
  , _eventInputCustomers :: [Key Customer]
  }
makeLenses ''EventInput

eventForm :: Maybe (T.Event) -> [(Key Customer,Text)] -> PhbForm T.Text EventInput
eventForm e cs = EventInput
  <$> "name"      .: check nameErrMsg isNotEmpty (text name)
  <*> "desc"      .: check descErrMsg isNotEmpty (text desc)
  <*> "impact"    .: check impactErrMsg isNotEmpty (text impact)
  <*> "start"     .: html5LocalTimeFormlet start
  <*> "finish"    .: html5OptLocalTimeFormlet finish
  <*> "status"    .: choice statusOpts status
  <*> "notes"     .: text notes
  <*> "customers" .: listOf (choice cs) customers
  where
    name    = e ^? _Just . T.eventName
    desc    = e ^? _Just . T.eventDesc . to T.unlines
    impact  = e ^? _Just . T.eventImpact . to T.unlines
    start  = e ^? _Just . T.eventDuration . _1
    finish  = e ^? _Just . T.eventDuration . _2 . _Just
    status = e ^? _Just . T.eventStatusLatest . eVal . eventStatusStatus
    notes = e ^? _Just . T.eventNotes . traversed . index 0 . eVal . eventNoteNote
    customers = e ^? _Just . T.eventCustomers . to (fmap entityKey)
    nameErrMsg = "Name must not be empty"
    descErrMsg = "What happened must not be empty"
    impactErrMsg = "Impact must not be empty"
    statusOpts = fmap (id &&& eventStatusHuman) [minBound..maxBound]

eventFormSplices :: PhbRuntimeSplice (Maybe T.Event) -> PhbSplice
eventFormSplices rts = do
  promise <- C.newEmptyPromise
  out <- C.withSplices
         C.runChildren
         ("eventForm" ## formSplice mempty mempty)
         (C.getPromise promise)
  pure . C.yieldRuntime $ do
    e  <- rts
    ct <- liftIO getCurrentTime
    cs <- lift . runPersist $
      (fmap customerChoiceOption <$> selectList [] [Asc CustomerName])
    (v, result) <- lift $ runForm "event" (eventForm e cs)

    case result of
      Just x  -> do
        tz <- liftIO getCurrentTimeZone
        lift (createEvent tz x ct (e ^? _Just . T.eventKey))
      Nothing -> C.putPromise promise v >> C.codeGen out
  where
    createEvent tz x cd kMay = do
      -- Should probably change this so that a DB error wouldn't just
      -- Crash us and should put a nice error in the form.
      case kMay of
       Nothing -> do
         void . runPersist $ do
           insert (newEvent tz x) >>= updateSatellites x cd
         flashSuccess $ "Timelog Created"
       Just k  -> do
         void . runPersist $ do
           replace k (newEvent tz x) >> updateSatellites x cd k
         flashSuccess $ "Timelog Updated"
      redirect "/events"
    newEvent tz (EventInput n d i st fn _ _ _) =
      Event n d i (localTimeToUTC tz st) (localTimeToUTC tz <$> fn)
    updateSatellites x cd k = do
      updateSatellite k (x ^. eventInputCustomers)
        EventCustomerEvent
        EventCustomerId
        EventCustomerCustomer
        eventCustomerCustomer
        EventCustomer

      insertTemporal k
        EventStatusStart
        EventStatusFinish
        EventStatusId
        EventStatusEvent
        (view eventStatusStatus)
        cd
        (EventStatus k cd Nothing (x ^. eventInputStatus))

      insertTemporal k
        EventNoteStart
        EventNoteFinish
        EventNoteId
        EventNoteEvent
        (view eventNoteNote)
        cd
        (EventNote k (x ^. eventInputNote) cd Nothing)

    customerChoiceOption (Entity k v) = (k,v ^. customerName)

eventRowSplice :: PhbRuntimeSplice [T.Event] -> PhbSplice
eventRowSplice =
  C.withSplices C.runChildren ("eventRow" ## rowSplice (ts <> ss))
  where
    ts = mapV (C.pureSplice . C.textSplice) $ do
      "name"          ## view T.eventName
      "status-class"  ## (^. T.eventStatusLatest . eVal . eventStatusStatus . to statusClass)
      "status"        ## (^. T.eventStatusLatest . eVal . eventStatusStatus . to eventStatusHuman)
      "duration"      ## (^. T.eventDuration . to durationText)
      "id"            ## (^. T.eventKey . to spliceKey)
    ss = do
      "customers"     ## spliceLines . fmap (^.. T.eventCustomers . traverse . eVal . customerName)
      "desc"          ## spliceLines . fmap (^. T.eventDesc)
      "impact"        ## spliceLines . fmap (^. T.eventImpact)
      "notes"         ## spliceLines . fmap (^. T.eventNoteLatest . eVal . eventNoteNote . to T.lines)
    statusClass EventSlaMet = "status-green"
    statusClass EventSlaJustMet = "status-amber"
    statusClass EventSlaNotMet   = "status-red"
    durationText (s,e) = spliceLocalTime s <> " to " <> (maybe "Ongoing" spliceLocalTime e)


listEventsSplices :: PhbSplice
listEventsSplices = C.withSplices C.runChildren splices rts
  where
    splices = do
      "activeEvents"    ## eventRowSplice . fmap fst
      "completedEvents" ## eventRowSplice . fmap snd
    rts = lift $ do
      ct <- liftIO $ getCurrentTime
      lt <- liftIO $ getLocalTime
      runPersist $ do
        ps <- selectList [] [Desc EventId]
        psW <- traverse (loadEvent ct) $ ps
        pure (partition (isActive lt) $ psW)

    isActive ct = maybe True (>= ct) . (^.T.eventDuration._2)

createEventSplices :: PhbSplice
createEventSplices = eventFormSplices (pure Nothing)

editEventSplices :: PhbSplice
editEventSplices = eventFormSplices . lift $ do
  ct <- liftIO getCurrentTime
  e  <- requireEntity "event" "id"
  Just <$> runPersist (loadEvent ct e)

allEventSplices :: Splices PhbSplice
allEventSplices = do
  "allEvents"   ## listEventsSplices
  "createEvent" ## createEventSplices
  "editEvent"   ## editEventSplices
