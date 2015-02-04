{-# LANGUAGE RankNTypes #-}
module Phb.Db.PublicHoliday where

import           BasePrelude
import           Prelude             ()

import           Control.Monad.Trans (MonadIO)
import           Data.Time           (Day)
import           Database.Esqueleto

import           Phb.Dates
import           Phb.Db.Internal

isPublicHoliday :: (MonadIO m,Functor m) => Day -> Db m Bool
isPublicHoliday cd = fmap (not . null) $ select $ from $ \ ph -> do
  where_ (ph ^. PublicHolidayDay ==. val cd )
  pure  ph

isBusinessDay :: (MonadIO m,Applicative m) => Day -> Db m Bool
isBusinessDay cd =
  bool (pure False) (not <$> isPublicHoliday cd) $ isWeekday cd

getLastBusinessDay
  :: (MonadIO m, Applicative m)
  => Day
  -> Db m Day
getLastBusinessDay cd = do
  let yd = prevWeekday cd
  ok <- isBusinessDay yd
  bool (getLastBusinessDay yd) (pure yd) ok
