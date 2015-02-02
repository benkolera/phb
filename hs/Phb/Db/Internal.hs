{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Phb.Db.Internal where

import BasePrelude hiding (delete, insert)
import Prelude     ()

import           Control.Lens                hiding (Action)
import           Control.Monad.Cont          (MonadIO)
import           Control.Monad.Reader        (ReaderT)
import qualified Data.Set                    as S
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Time                   (Day, UTCTime (..))
import           Database.Persist
import           Database.Persist.Postgresql hiding (getJust)
import           Database.Persist.TH

import Phb.Db.Enums

share
  [mkPersist sqlSettings { mpsGenerateLenses = True }
  , mkMigrate "migrateAll"
  ] [persistLowerCase|
Person
  name              Text
  email             Text
  department        Text
  receivesHeartbeat Bool
  logsTime          Bool
  UniquePersonName name
  deriving Show
Customer
  name Text
  UniqueCustomerName name
  deriving Show
Action
  name      Text
  due       Day
  deriving Show
ActionCustomer
  action   ActionId
  customer CustomerId
  UniqueActionCustomer action customer
  deriving Show
ActionNote
  action    ActionId
  note      Text
  start     UTCTime
  finish    UTCTime Maybe
  deriving Show
ActionStatus
  action    ActionId
  status    Text
  completed Bool
  start     UTCTime
  finish    UTCTime Maybe
  deriving Show
ActionPerson
  action ActionId
  person PersonId
  UniqueActionPerson action person
  deriving Show
Backlog
  name     Text
  priority Int
  deriving Show
BacklogCustomer
  backlog   BacklogId
  customer CustomerId
  UniqueBacklogCustomer backlog customer
  deriving Show
BacklogStatus
  backlog  BacklogId
  start    UTCTime
  finish   UTCTime Maybe
  status   BacklogStatusEnum
  deriving Show
  deriving Eq
BacklogNote
  backlog BacklogId
  note    Text
  start   UTCTime
  finish  UTCTime Maybe
  deriving Show
  deriving Eq
BacklogPerson
  backlog BacklogId
  person PersonId
  UniqueBacklogPerson backlog person
  deriving Show
Event
  name   Text
  desc   Text
  impact Text
  start  UTCTime
  finish UTCTime Maybe
  deriving Show
EventCustomer
  event   EventId
  customer CustomerId
  UniqueEventCustomer event customer
  deriving Show
EventNote
  event  EventId
  note   Text
  start  UTCTime
  finish UTCTime Maybe
  deriving Show
  deriving Eq
EventStatus
  event  EventId
  start  UTCTime
  finish UTCTime Maybe
  status EventStatusEnum
  deriving Show
  deriving Eq
Heartbeat
  start           Day
  finish          Day
  upcomingEvents  Text
  highlights      Text
  deriving Show
HeartbeatAction
  heartbeat HeartbeatId
  action    ActionId
  UniqueHeartbeatAction heartbeat action
  deriving Show
HeartbeatBacklog
  heartbeat HeartbeatId
  backlog   BacklogId
  UniqueHeartbeatBacklog heartbeat backlog
  deriving Show
HeartbeatEvent
  heartbeat HeartbeatId
  event     EventId
  UniqueHeartbeatEvent heartbeat event
  deriving Show
HeartbeatProject
  heartbeat HeartbeatId
  project   ProjectId
  UniqueHeartbeatProject heartbeat project
  deriving Show
Project
  name     Text
  priority Int
  started  Day
  finished Day Maybe
  deriving Show
ProjectCustomer
  project   ProjectId
  customer  CustomerId
  UniqueProjectCustomer project customer
  deriving Show
ProjectStatus
  project ProjectId
  start   UTCTime
  finish  UTCTime Maybe
  phase   Text
  colour  StatusColourEnum
  desc    Text Maybe
  deriving Show
  deriving Eq
ProjectNote
  project ProjectId
  note    Text
  start   UTCTime
  finish  UTCTime Maybe
  deriving Show
  deriving Eq
ProjectPerson
  project ProjectId
  person  PersonId
  UniqueProjectPerson project person
  deriving Show
ProjectTargetDate
  project  ProjectId
  desc     Text
  day      Day
  handwavy Text Maybe
  deriving Show
Success
  heartbeat    HeartbeatId
  what         Text
  achievements Text
  deriving Show
SuccessPerson
  success SuccessId
  person  PersonId
  UniqueSuccessPerson success person
  deriving Show
Task
  person   PersonId
  name     Text
  start    Day
  finish   Day Maybe
  project  ProjectId Maybe
  event    EventId   Maybe
  backlog  BacklogId Maybe
  action   ActionId  Maybe
  category WorkCategoryId Maybe
TimeLog
  person   PersonId
  desc     Text
  minutes  Int
  day      Day
  task     TaskId
  deriving Show
WorkCategory
  name       Text
  UniqueWorkCategoryName name
  deriving Show
PersonLogin
  person PersonId
  login Text
  activatedAt UTCTime Maybe default=now()
  suspendedAt UTCTime Maybe default=now()
  rememberToken Text Maybe
  loginCount Int default=0
  failedLoginCount Int default=0
  lockedOutUntil UTCTime Maybe default=now()
  currentLoginAt UTCTime Maybe default=now()
  lastLoginAt UTCTime Maybe default=now()
  currentIp Text Maybe
  lastIp Text Maybe
  createdAt UTCTime default=now()
  updatedAt UTCTime default=now()
  resetToken Text Maybe
  resetRequestedAt UTCTime Maybe
  roles String default=''
  meta String default=''
|]

type Db m a = (MonadIO m) => SqlPersistT m a

eVal :: Getter (Entity v) v
eVal = to entityVal
eKey :: Getter (Entity v) (Key v)
eKey = to entityKey

getEntity
  :: (PersistStore (PersistEntityBackend a), PersistEntity a, MonadIO m, Functor m)
  => Key a
  -> ReaderT (PersistEntityBackend a) m (Maybe (Entity a))
getEntity k = fmap (Entity k) <$> get k

getEntityJust
  :: (PersistStore (PersistEntityBackend a), PersistEntity a, MonadIO m, Functor m)
  => Key a
  -> ReaderT (PersistEntityBackend a) m (Entity a)
getEntityJust k = (Entity k) <$> getJust k

selectLatestNote
  :: (PersistQuery (PersistEntityBackend v), PersistEntity v, PersistField k, MonadIO m)
  => EntityField v k
  -> EntityField v UTCTime
  -> Getting Text v Text
  -> k
  -> Day
  -> ReaderT (PersistEntityBackend v) m [Text]
selectLatestNote nk dk nl k d = do
 n <- selectFirst [nk ==. k,dk >=. UTCTime d 0] [Desc dk]
 return . unNote . fmap (view nl . entityVal) $ n

unNote :: Maybe Text -> [Text]
unNote = maybe [] T.lines

updateSatellite
  :: ( PersistQuery (PersistEntityBackend s)
    , PersistEntity s
    , PersistField a
    , PersistField fk
    , MonadIO m
    , Applicative m
    , Ord a
    )
  => fk
  -> [a]
  -> EntityField s fk
  -> EntityField s id
  -> EntityField s a
  -> Getter s a
  -> (fk -> a -> s)
  -> ReaderT (PersistEntityBackend s) m ()
updateSatellite fk ri fkCol idCol refCol refLens con = do
  ecs <- selectList [fkCol ==. fk] [Asc idCol]
  let (toRem,toAdd) = diff (fmap (^. eVal . refLens) ecs) ri
  deleteWhere [fkCol ==. fk,refCol <-. toList toRem]
  insertMany_ . fmap (con fk) . toList $ toAdd

diff :: Ord a => [a] -> [a] -> (S.Set a, S.Set a)
diff oldList newList = (toRem,toAdd)
  where
    old = S.fromList oldList
    new = S.fromList newList
    toRem = S.difference old new
    toAdd = S.difference new old

paginate :: Int64 -> Int64 -> (Int64,Int64)
paginate p pw = (pw,(p-1)*pw)

paginateOpts :: (Int64,Int64) -> [SelectOpt record]
paginateOpts (l,o) = [LimitTo $ fromIntegral l,OffsetBy $ fromIntegral o]
