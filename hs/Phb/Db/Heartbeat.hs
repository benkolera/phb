{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE TemplateHaskell   #-}
module Phb.Db.Heartbeat where

import BasePrelude hiding (insert)
import Prelude     ()

import           Control.Lens               hiding (Action)
import           Control.Monad.IO.Class     (MonadIO)
import           Control.Monad.Trans.Reader (ReaderT)
import qualified Data.Map                   as M
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Time                  (Day, UTCTime (..))
import           Database.Persist
import           Database.Persist.Sql       (SqlBackend)

import           Phb.Db.Action
import           Phb.Db.Backlog
import           Phb.Db.Enums        ()
import           Phb.Db.Event
import           Phb.Db.Internal
import           Phb.Db.Person
import           Phb.Db.Project
import           Phb.Db.Success
import           Phb.Db.TimeLog
import qualified Phb.Types.Heartbeat as T
import qualified Phb.Types.Success   as T
import qualified Phb.Types.TimeLog   as T

data HeartbeatInput = HeartbeatInput
  { _heartbeatInputStart      :: Day
  , _heartbeatInputFinish     :: Day
  , _heartbeatInputUpcoming   :: [Text]
  , _heartbeatInputHighlights :: [Text]
  , _heartbeatInputSuccesses  :: [HeartbeatSuccessInput]
  , _heartbeatInputActions    :: [Key Action]
  , _heartbeatInputBacklog    :: [Key Backlog]
  , _heartbeatInputEvents     :: [Key Event]
  , _heartbeatInputProjects   :: [Key Project]
  }
makeLenses ''HeartbeatInput

loadHeartbeat :: (MonadIO m, Applicative m) => Entity Heartbeat -> Db m T.Heartbeat
loadHeartbeat (Entity hId h) = do
  let s  = h^.heartbeatStart
  let f  = h^.heartbeatFinish
  let ct = UTCTime f 86400
  ps  <- loadRel HeartbeatProjectHeartbeat heartbeatProjectProject (loadProject ct)
  es  <- loadRel HeartbeatEventHeartbeat heartbeatEventEvent (loadEvent ct)
  bs  <- loadRel HeartbeatBacklogHeartbeat heartbeatBacklogBacklog (loadBacklog ct)
  as  <- loadRel HeartbeatActionHeartbeat heartbeatActionAction (loadAction ct)
  sss <- selectList [SuccessHeartbeat          ==. hId] []
  ss  <- traverse success sss
  stl <- loadTimeLogsForPeriod s f
  prev <- fmap entityKey <$> lastHeartbeat s
  next <- fmap entityKey <$> nextHeartbeat f
  return $ T.Heartbeat
    hId
    prev
    next
    (h ^. heartbeatStart)
    (h ^. heartbeatFinish)
    (T.lines . view heartbeatUpcomingEvents $ h)
    (T.lines . view heartbeatHighlights $ h)
    ss
    ps
    bs
    es
    as
    (supportLogSummary stl)

  where
    success (Entity sId (Success _ what achievments)) = do
      ps <- loadRelatedPeople SuccessPersonPerson SuccessPersonSuccess sId
      return $ T.Success
        sId
        ps
        what
        (T.lines achievments)
    loadRel relHbCol relIdL load = do
      rels <- selectList [relHbCol ==. hId] []
      traverse ((load =<<) . getEntityJust . (^. eVal . relIdL)) rels

    supportLogSummary =
      fmap (\ (k,(hs,ps)) -> T.HeartbeatTimeLog k (getSum hs) ps)
      . sortBy (compare `on` (fst . snd))
      . M.toList
      . foldl' accumTimeLogSummaryMap M.empty

    accumTimeLogSummaryMap m tls =
      M.insertWith (<>) (supportLogSummaryLabel tls) (supportLogSummaryVal tls) m

    supportLogSummaryVal (T.TimeLogWhole tl p _) =
      ( tl^.eVal.timeLogMinutes.to fromIntegral.to (/60.0).to Sum
      , [p]
      )

    supportLogSummaryLabel = (^.T.timeLogWholeLink._Just.T.timeLogLinkName)

nextHeartbeat
  :: (MonadIO m, Functor m)
  => Day
  -> ReaderT SqlBackend m (Maybe (Entity Heartbeat))
nextHeartbeat cd =
  selectFirst [HeartbeatStart >=. cd] [Asc HeartbeatFinish]

lastHeartbeat
  :: (MonadIO m, Functor m)
  => Day
  -> ReaderT SqlBackend m (Maybe (Entity Heartbeat))
lastHeartbeat cd =
  selectFirst [HeartbeatFinish <=. cd] [Desc HeartbeatFinish]

loadCustomerNames :: (MonadIO m, Functor m)
  => [Key Customer]
  -> ReaderT SqlBackend m [Text]
loadCustomerNames cIds =
  fmap (view customerName . entityVal)
  <$> selectList  [CustomerId <-. cIds] []

loadPeopleNames :: (MonadIO m, Functor m)
  => [Key Person]
  -> ReaderT SqlBackend m [Text]
loadPeopleNames pIds =
  fmap (view personName . entityVal)
  <$> selectList  [PersonId <-. pIds] []

upsertHeartbeat :: (Applicative m, MonadIO m) => HeartbeatInput -> Maybe (Key Heartbeat) -> Db m (Key Heartbeat)
upsertHeartbeat x kMay = do
  k <- case kMay of
   Nothing -> insert (newHeartbeat x)
   Just k  -> replace k (newHeartbeat x) >> return k
  updateSatellites k
  return k
  where
    newHeartbeat (HeartbeatInput s f u h _ _ _ _ _) =
      Heartbeat s f (T.unlines u) (T.unlines h)

    updateSatellites k = do
      upsertHeartbeatSuccesses k (x^.heartbeatInputSuccesses)
      updateSatellite k (x ^. heartbeatInputActions)
        HeartbeatActionHeartbeat
        HeartbeatActionId
        HeartbeatActionAction
        heartbeatActionAction
        HeartbeatAction

      updateSatellite k (x ^. heartbeatInputBacklog)
        HeartbeatBacklogHeartbeat
        HeartbeatBacklogId
        HeartbeatBacklogBacklog
        heartbeatBacklogBacklog
        HeartbeatBacklog

      updateSatellite k (x ^. heartbeatInputEvents)
        HeartbeatEventHeartbeat
        HeartbeatEventId
        HeartbeatEventEvent
        heartbeatEventEvent
        HeartbeatEvent

      updateSatellite k (x ^. heartbeatInputProjects)
        HeartbeatProjectHeartbeat
        HeartbeatProjectId
        HeartbeatProjectProject
        heartbeatProjectProject
        HeartbeatProject
