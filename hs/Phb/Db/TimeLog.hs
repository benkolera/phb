{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TupleSections    #-}
module Phb.Db.TimeLog where

import           BasePrelude           hiding (on)
import           Prelude               ()

import qualified Control.Lens          as L
import           Control.Monad.Trans   (MonadIO)
import qualified Data.Function         as F
import qualified Data.Map              as M
import           Data.Time             (Day)
import           Database.Esqueleto

import           Phb.Dates
import           Phb.Db.Esqueleto
import           Phb.Db.Internal
import           Phb.Db.Task
import           Phb.Types.Task
import           Phb.Types.TimeLog
import           Phb.Types.TimeSummary

queryTimeLogs
  :: (MonadIO m, Applicative m)
  => Maybe Period
  -> [Key Person]
  -> Maybe (Int64,Int64)
  -> Db m [TimeLogWhole]
queryTimeLogs pp ups pgs = do
  twes <- select $ from $ \ tl -> do
    where_ (mkFilter tl)
    traverse_ (\ (l,o) -> limit l >> offset o) pgs
    orderBy [ desc $ tl ^. TimeLogDay ]
    return tl
  traverse loadTimeLogWhole twes
  where
    mkFilter tl = foldl (&&.) (val True) . catMaybes $
      [ fmap (mkPeriodFilter tl) pp
      , mfilter (const (not . null $ ups)) . Just $ tl ^. TimeLogPerson `in_` valList ups
      ]
    mkPeriodFilter tl (ForMonth m) =
      withinPeriod tl TimeLogDay (startOfMonth m) (endOfMonth m)
    mkPeriodFilter tl (ForWeek w) =
      withinPeriod tl TimeLogDay (startOfWeek w) (endOfWeek w)
    mkPeriodFilter tl (ForDay d) = (tl ^. TimeLogDay ==. val d)

loadTimeLogWhole
  :: (MonadIO m, Applicative m)
  => Entity TimeLog
  -> Db m TimeLogWhole
loadTimeLogWhole twe = do
  t  <- getEntityJust $ twe L.^.eVal.timeLogTask
  tw <- loadTaskWhole t
  pure (TimeLogWhole twe tw)

loadTimeLogsForPeriod
  :: (MonadIO m, Applicative m)
  => Day
  -> Day
  -> Db m [TimeLogWhole]
loadTimeLogsForPeriod s f = logs >>= traverse loadTimeLogWhole
  where
    logs =
      select $ from $ \ (tl) -> do
        where_ (tl ^. TimeLogDay >=. val s &&. tl ^. TimeLogDay <=. val f)
        return tl

summaryForTaskForDay
  :: (MonadIO m, Applicative m)
  => TaskWhole
  -> Day
  -> Db m TimeSummary
summaryForTaskForDay t cd = summary . fmap timeLogHours <$> logs
  where
    logs =
      select $ from $ \ (tl) -> do
        where_ (tl ^. TimeLogDay ==. val cd
                &&. tl ^. TimeLogTask ==. val (t L.^.taskWholeTask.eKey))
        return tl
    summary hs = TimeSummary
      (t L.^.taskWholeLink.L._Just.taskLinkName)
      (sum hs)
      [t L.^.taskWholePerson]



summariseTimeLogs :: [TimeLogWhole] -> [TimeSummary]
summariseTimeLogs =
  fmap (\ (k,(hs,ps)) -> TimeSummary k (getSum hs) ps)
  . reverse
  . sortBy (compare `F.on` (fst . snd))
  . M.toList
  . foldl' accumTimeLogSummaryMap M.empty

  where
    accumTimeLogSummaryMap m tls =
      M.insertWith (<>) (summaryLabel tls) (summaryVal tls) m

    summaryVal tl =
      ( Sum (timeLogHours $ tl L.^.timeLogWholeLog )
      , [tl L.^.timeLogWholeTask.taskWholePerson]
      )

    summaryLabel = (L.^.timeLogWholeTask.taskWholeLink.L._Just.taskLinkName)

timeLogHours :: Entity TimeLog -> Double
timeLogHours =
  (L.^.eVal.timeLogMinutes.L.to(fromIntegral >>> (/60.0)))
