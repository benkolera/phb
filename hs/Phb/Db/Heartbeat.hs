{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE TemplateHaskell   #-}
module Phb.Db.Heartbeat where

import BasePrelude hiding (insert, on)
import Prelude     ()

import           Control.Lens               hiding (Action, from, (^.))
import qualified Control.Lens               as L
import           Control.Monad.IO.Class     (MonadIO)
import           Control.Monad.Trans.Reader (ReaderT)
import qualified Data.Function              as F
import qualified Data.Map                   as M
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Time                  (Day, UTCTime (..))
import           Database.Esqueleto
import qualified Database.Persist           as P

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
  let s  = h L.^.heartbeatStart
  let f  = h L.^.heartbeatFinish
  let ct = UTCTime f 86400
  ps  <- loadRel HeartbeatProjectHeartbeat heartbeatProjectProject HeartbeatProjectProject ProjectId (^.ProjectPriority) (loadProject ct)
  es  <- loadRel HeartbeatEventHeartbeat heartbeatEventEvent HeartbeatEventEvent EventId (^.EventId) (loadEvent ct)
  bs  <- loadRel HeartbeatBacklogHeartbeat heartbeatBacklogBacklog HeartbeatBacklogBacklog BacklogId (^.BacklogPriority) (loadBacklog ct)
  as  <- loadRel HeartbeatActionHeartbeat heartbeatActionAction HeartbeatActionAction ActionId (^.ActionId) (loadAction ct)
  sss <- P.selectList [SuccessHeartbeat P.==. hId] []
  ss  <- traverse success sss
  stl <- loadTimeLogsForPeriod s f
  prev <- fmap entityKey <$> lastHeartbeat s
  next <- fmap entityKey <$> nextHeartbeat f
  return $ T.Heartbeat
    hId
    prev
    next
    (h L.^. heartbeatStart)
    (h L.^. heartbeatFinish)
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
    loadRel relHbCol relIdL relXFk xId priorityCol load = do
      rels <- select $ from $ \ (rel `InnerJoin` x) -> do
        on (rel^.relXFk ==. x^.xId)
        where_ (rel^.relHbCol ==. val hId)
        orderBy [desc $ priorityCol x]
        return rel
      traverse ((load =<<) . getEntityJust . (L.^. eVal . relIdL)) rels

    supportLogSummary =
      fmap (\ (k,(hs,ps)) -> T.HeartbeatTimeLog k (getSum hs) ps)
      . reverse
      . sortBy (compare `F.on` (fst . snd))
      . M.toList
      . foldl' accumTimeLogSummaryMap M.empty

    accumTimeLogSummaryMap m tls =
      M.insertWith (<>) (supportLogSummaryLabel tls) (supportLogSummaryVal tls) m

    supportLogSummaryVal (T.TimeLogWhole tl p _) =
      ( tl L.^.eVal.timeLogMinutes.to fromIntegral.to (/60.0).to Sum
      , [p]
      )

    supportLogSummaryLabel = (L.^.T.timeLogWholeLink._Just.T.timeLogLinkName)

nextHeartbeat
  :: (MonadIO m, Functor m)
  => Day
  -> ReaderT SqlBackend m (Maybe (Entity Heartbeat))
nextHeartbeat cd =
  P.selectFirst [HeartbeatStart P.>=. cd] [P.Asc HeartbeatFinish]

lastHeartbeat
  :: (MonadIO m, Functor m)
  => Day
  -> ReaderT SqlBackend m (Maybe (Entity Heartbeat))
lastHeartbeat cd =
  P.selectFirst [HeartbeatFinish P.<=. cd] [P.Desc HeartbeatFinish]

loadCustomerNames :: (MonadIO m, Functor m)
  => [Key Customer]
  -> ReaderT SqlBackend m [Text]
loadCustomerNames cIds =
  fmap (view customerName . entityVal)
  <$> P.selectList  [CustomerId P.<-. cIds] []

loadPeopleNames :: (MonadIO m, Functor m)
  => [Key Person]
  -> ReaderT SqlBackend m [Text]
loadPeopleNames pIds =
  fmap (view personName . entityVal)
  <$> P.selectList  [PersonId P.<-. pIds] []

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
      upsertHeartbeatSuccesses k (x L.^.heartbeatInputSuccesses)
      updateSatellite k (x L.^.heartbeatInputActions)
        HeartbeatActionHeartbeat
        HeartbeatActionId
        HeartbeatActionAction
        heartbeatActionAction
        HeartbeatAction

      updateSatellite k (x L.^. heartbeatInputBacklog)
        HeartbeatBacklogHeartbeat
        HeartbeatBacklogId
        HeartbeatBacklogBacklog
        heartbeatBacklogBacklog
        HeartbeatBacklog

      updateSatellite k (x L.^. heartbeatInputEvents)
        HeartbeatEventHeartbeat
        HeartbeatEventId
        HeartbeatEventEvent
        heartbeatEventEvent
        HeartbeatEvent

      updateSatellite k (x L.^. heartbeatInputProjects)
        HeartbeatProjectHeartbeat
        HeartbeatProjectId
        HeartbeatProjectProject
        heartbeatProjectProject
        HeartbeatProject
