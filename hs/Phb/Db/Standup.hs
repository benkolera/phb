{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TupleSections    #-}
module Phb.Db.Standup where

import           BasePrelude          hiding (on)
import           Prelude              ()

import qualified Control.Lens         as L
import           Control.Monad.Trans  (MonadIO)
import           Data.Time            (Day)
import           Database.Esqueleto

import           Phb.Db.Internal
import           Phb.Db.PublicHoliday
import           Phb.Db.Task
import           Phb.Db.TimeLog
import           Phb.Types.Standup
import           Phb.Types.Task
import           Phb.Util

loadStandupForDay :: (MonadIO m, Applicative m) => Day -> Db m [StandupPersonSummary]
loadStandupForDay td = do
  yd <- getLastBusinessDay td
  yt <- loadActiveTasksForDay (const $ val True) yd
  tt <- loadActiveTasksForDay (const $ val True) td
  ys <- traverse (yesterdaySummary yd) yt
  let ts = fmap todaySummary tt
  pure . fmap mergeSummaries . partitionOn (L.^.standupPerson.eKey) $ ys ++ ts
  where
    mergeSummaries = foldl1 mergeSummary
    mergeSummary (StandupPersonSummary p y1 t1) (StandupPersonSummary _ y2 t2) =
      StandupPersonSummary p (y1 ++ y2) (t1 ++ t2)
    yesterdaySummary yd tw = do
      ts <- summaryForTaskForDay tw yd
      let c = maybe False (<= yd) $ tw L.^.taskWholeTask.eVal.taskFinish
      pure $ StandupPersonSummary (tw L.^.taskWholePerson) [(tw,ts,c)] []
    todaySummary tw = StandupPersonSummary (tw L.^.taskWholePerson) [] [tw]
