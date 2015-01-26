{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TupleSections    #-}
module Phb.Db.TimeLog where

import BasePrelude hiding (on)
import Prelude     ()

import qualified Control.Lens        as L
import           Control.Monad.Trans (MonadIO)
import qualified Data.Function       as F
import qualified Data.Map            as M
import           Data.Time           (Day)
import           Database.Esqueleto

import Phb.Dates
import Phb.Db.Esqueleto
import Phb.Db.Internal
import Phb.Types.TimeLog
import Phb.Types.TimeSummary

data TimeLogPeriod
  = TimeLogsForMonth Month
  | TimeLogsForWeek Week
  | TimeLogsForDay Day
L.makePrisms ''TimeLogPeriod

queryTimeLogs
  :: (MonadIO m, Applicative m)
  => Maybe TimeLogPeriod
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
    mkPeriodFilter tl (TimeLogsForMonth m) =
      withinPeriod tl TimeLogDay (startOfMonth m) (endOfMonth m)
    mkPeriodFilter tl (TimeLogsForWeek w) =
      withinPeriod tl TimeLogDay (startOfWeek w) (endOfWeek w)
    mkPeriodFilter tl (TimeLogsForDay d) = (tl ^. TimeLogDay ==. val d)

loadTimeLogWhole
  :: (MonadIO m, Applicative m)
  => Entity TimeLog
  -> Db m TimeLogWhole
loadTimeLogWhole twe = do
  let tw = entityVal twe
  p  <- getJust $ tw L.^. timeLogPerson
  pLink <- loadLink tw projectName timeLogProject ProjectLink
  eLink <- loadLink tw eventName timeLogEvent EventLink
  bLink <- loadLink tw backlogName timeLogBacklog BacklogLink
  aLink <- loadLink tw actionName timeLogAction ActionLink
  wcLink <- loadLink tw workCategoryName timeLogCategory WorkCategoryLink
  let lLink = pLink <|> eLink <|> bLink <|> aLink <|> wcLink
  pure (TimeLogWhole twe (Entity (tw L.^.timeLogPerson) p) lLink)
  where
    loadLink tw nl ll lc =
      fmap (\ (k,v) -> TimeLogLink (v L.^. nl) (lc k))
      <$>  traverse (\ k -> (k,) <$> getJust k) (tw L.^. ll)

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

    summaryVal (TimeLogWhole tl p _) =
      ( tl L.^.eVal.timeLogMinutes.L.to(fromIntegral >>> (/60.0) >>> Sum)
      , [p]
      )

    summaryLabel = (L.^.timeLogWholeLink.L._Just.timeLogLinkName)

mkLinkOptions
  :: Functor f
  => (Key r -> l)
  -> L.Getting n r n
  -> f (Entity r)
  -> f (l, n)
mkLinkOptions lc nl =
  fmap (lc . entityKey &&& L.view nl . entityVal)
