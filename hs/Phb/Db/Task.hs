{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TupleSections    #-}
module Phb.Db.Task where

import BasePrelude hiding (on)
import Prelude     ()

import qualified Control.Lens        as L
import           Control.Monad.Trans (MonadIO)
import           Data.Time           (Day)
import           Database.Esqueleto

import Phb.Db.Esqueleto
import Phb.Db.Internal
import Phb.Types.Task

loadTaskWhole
  :: (MonadIO m, Applicative m)
  => Entity Task
  -> Db m TaskWhole
loadTaskWhole twe = do
  let tw = entityVal twe
  p  <- getJust $ tw L.^. taskPerson
  pLink <- loadLink tw projectName taskProject ProjectLink
  eLink <- loadLink tw eventName taskEvent EventLink
  bLink <- loadLink tw backlogName taskBacklog BacklogLink
  aLink <- loadLink tw actionName taskAction ActionLink
  wcLink <- loadLink tw workCategoryName taskCategory WorkCategoryLink
  let lLink = pLink <|> eLink <|> bLink <|> aLink <|> wcLink
  pure (TaskWhole twe (Entity (tw L.^.taskPerson) p) lLink)
  where
    loadLink tw nl ll lc =
      fmap (\ (k,v) -> TaskLink (v L.^. nl) (lc k))
      <$>  traverse (\ k -> (k,) <$> getJust k) (tw L.^. ll)

loadTasksForPeriod
  :: (MonadIO m, Applicative m)
  => Day
  -> Day
  -> Db m [TaskWhole]
loadTasksForPeriod s f = logs >>= traverse loadTaskWhole
  where
    logs =
      select $ from $ \ (tl) -> do
        where_ (overlapsPeriod tl TaskStart TaskFinish s f)
        return tl

loadTasksForPeopleForDay
  :: (MonadIO m, Applicative m)
  => [Key Person]
  -> Day
  -> Db m [TaskWhole]
loadTasksForPeopleForDay pks d = logs >>= traverse loadTaskWhole
  where
    logs =
      select $ from $ \ (tl) -> do
        where_
          (   tl ^. TaskPerson `in_` valList pks
          &&. (withinBounds tl TaskStart TaskFinish d)
          )
        return tl

loadActiveTasksForDay
  :: (MonadIO m, Applicative m)
  => (SqlExpr (Entity Task) -> SqlExpr (Value Bool))
  -> Day
  -> Db m [TaskWhole]
loadActiveTasksForDay q cd = logs >>= traverse loadTaskWhole
  where
    logs = select $ from $ \ (t) -> do
      where_ ( q t &&. isActiveQ t cd )
      return t

loadCompletedTasksForDay
  :: (MonadIO m, Applicative m)
  => (SqlExpr (Entity Task) -> SqlExpr (Value Bool))
  -> Day
  -> Db m [TaskWhole]
loadCompletedTasksForDay q cd = logs >>= traverse loadTaskWhole
  where
    logs = select $ from $ \ (t) -> do
      where_ ( q t &&. not_ (isActiveQ t cd) &&. t ^. TaskStart <. val cd )
      orderBy [desc $ t ^. TaskFinish ]
      limit 25
      return t

isActiveQ
  :: Esqueleto query expr backend
  => expr (Entity Task)
  -> Day
  -> expr (Value Bool)
isActiveQ tl d = withinBounds tl TaskStart TaskFinish d

mkLinkOptions
  :: Functor f
  => (Key r -> l)
  -> L.Getting n r n
  -> f (Entity r)
  -> f (l, n)
mkLinkOptions lc nl =
  fmap (lc . entityKey &&& L.view nl . entityVal)
