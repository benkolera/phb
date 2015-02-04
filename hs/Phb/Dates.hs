{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Phb.Dates
  ( DayOfWeek(..)
  , Week
  , Month
  , Period(..)
  , getCurrentDay
  , getLocalTime
  , localDayFromUTC
  , localDayToUTC
  , fromGregorianTime
  , isWeekday
  , isWeekDayOfWeek
  , parseDay
  , parseHumanDay
  , dayOfWeek
  , prevWeekday
  , nextWeekday
  , weekOfDay
  , mkWeek
  , parseWeek
  , parseHumanWeek
  , startOfWeek
  , endOfWeek
  , prevWeek
  , nextWeek
  , monthOfDay
  , mkMonth
  , parseMonth
  , parseHumanMonth
  , startOfMonth
  , endOfMonth
  , prevMonth
  , nextMonth
  , weekYear
  , weekNum
  , monthYear
  , monthNum
  , parseHumanPeriod
  , _ForMonth
  , _ForWeek
  , _ForDay
  ) where

import           BasePrelude
import           Prelude                     ()

import           Control.Error
import           Control.Lens
import           Data.Time
import           Data.Time.Calendar.MonthDay
import           Data.Time.Calendar.WeekDate
import           Data.Time.Lens
import           System.Locale               (defaultTimeLocale)
import           Text.Printf                 (printf)

getCurrentDay :: IO Day
getCurrentDay = localDay . zonedTimeToLocalTime <$> getZonedTime

getLocalTime :: IO LocalTime
getLocalTime = zonedTimeToLocalTime <$> getZonedTime

localDayFromUTC :: UTCTime -> IO Day
localDayFromUTC ct = getCurrentTimeZone
  <&> (localDay . zonedTimeToLocalTime . (`utcToZonedTime` ct))

localDayToUTC :: Day -> IO UTCTime
localDayToUTC d = do
  tz <- getCurrentTimeZone
  pure . localTimeToUTC tz . LocalTime d $ midnight

--showTextDay :: Day -> T.Text
--showTextDay = T.pack . formatTime defaultTimeLocale "%F"

fromGregorianTime :: Integer -> Int -> Int -> Int -> Int -> Pico -> LocalTime
fromGregorianTime y m d hh mm ss =
  LocalTime (fromGregorian y m d) (TimeOfDay hh mm ss)

parseDay :: String -> Maybe Day
parseDay = parseTime defaultTimeLocale "%F"

parseHumanDay :: String -> IO (Maybe Day)
parseHumanDay "today"     = Just <$> getCurrentDay
parseHumanDay "yesterday" = Just . addDays (-1) <$> getCurrentDay
parseHumanDay s           = pure $ parseDay s

data DayOfWeek
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday

dayOfWeek :: Day -> DayOfWeek
dayOfWeek = fromInt . view _3 . toWeekDate
  where
    fromInt 1 = Monday
    fromInt 2 = Tuesday
    fromInt 3 = Wednesday
    fromInt 4 = Thursday
    fromInt 5 = Friday
    fromInt 6 = Saturday
    fromInt 7 = Sunday
    fromInt d = error $ "Invalid day in Day value: " ++ show d

isWeekday :: Day -> Bool
isWeekday = isWeekDayOfWeek . dayOfWeek

isWeekDayOfWeek :: DayOfWeek -> Bool
isWeekDayOfWeek Saturday = False
isWeekDayOfWeek Sunday   = False
isWeekDayOfWeek _        = True

adjustByDay :: (DayOfWeek -> Integer) -> Day -> Day
adjustByDay f d = flip addDays d . f . dayOfWeek $ d

-- |
-- >>> prevWeekday (fromGregorian 2015 1 28)
-- 2015-01-27
-- >>> prevWeekday (fromGregorian 2015 1 26)
-- 2015-01-23
-- >>> prevWeekday (fromGregorian 2015 1 25)
-- 2015-01-23
-- >>> nextWeekday (fromGregorian 2015 1 28)
-- 2015-01-29
-- >>> nextWeekday (fromGregorian 2015 1 23)
-- 2015-01-26
-- >>> nextWeekday (fromGregorian 2015 1 24)
-- 2015-01-26
nextWeekday,prevWeekday :: Day -> Day
prevWeekday = adjustByDay daysTill
  where
    daysTill Monday = (-3)
    daysTill Sunday = (-2)
    daysTill _      = (-1)
nextWeekday = adjustByDay daysTill
  where
    daysTill Friday   = 3
    daysTill Saturday = 2
    daysTill _        = 1

data Week = Week { _weekYear :: Integer, _weekNum :: Int }
makeLenses ''Week

instance Show Week where
  show (Week y w) = show y ++ "-W" ++ printf "%02d" w

between :: Ord a => a -> a -> a -> Bool
between mn mx a = mn <= a && mx >= a

-- |
-- >>> mkWeek 2015 1
-- Just 2015-W01
-- >>> mkWeek 2015 53
-- Just 2015-W53
-- >>> mkWeek 2015 0
-- Nothing
-- >>> mkWeek 2015 54
-- Nothing
mkWeek :: Integer -> Int -> Maybe Week
mkWeek y w = Week <$> pure y <*> mfilter (between 1 53) (Just w)

-- |
-- >>> parseWeek "2015-W01"
-- Just 2015-W01
-- >>> parseWeek "2015-W1"
-- Just 2015-W01
-- >>> parseWeek "2015-W53"
-- Just 2015-W53
-- >>> parseWeek "2015-W55"
-- Nothing
-- >>> parseWeek "2015-12"
-- Nothing
parseWeek :: String -> Maybe Week
parseWeek s = join $ mkWeek <$> y <*  sep <*>  w
  where
    y = readMay . take 4 $ s
    sep  = mfilter (== "-W") . Just . take 2 . drop 4 $ s
    w = readMay . drop 6 $ s

parseHumanWeek :: String -> IO (Maybe Week)
parseHumanWeek "this_week" = Just . weekOfDay <$> getCurrentDay
parseHumanWeek "last_week" = Just . prevWeek . weekOfDay <$> getCurrentDay
parseHumanWeek s           = pure $ parseWeek s

-- |
-- >>> weekOfDay (fromGregorian 2015 1 25)
-- 2015-W04
-- >>> weekOfDay (fromGregorian 2015 12 31)
-- 2015-W53
weekOfDay :: Day -> Week
weekOfDay = (\ (y,w,_) -> Week y w) . toWeekDate

-- |
-- >>> startOfWeek (Week 2015 4)
-- 2015-01-19
-- >>> endOfWeek (Week 2015 4)
-- 2015-01-25
startOfWeek,endOfWeek :: Week -> Day
startOfWeek (Week y w) = fromWeekDate y w 1
endOfWeek   (Week y w) = fromWeekDate y w 7

-- |
-- >>> prevWeek (Week 2015 4)
-- 2015-W03
-- >>> nextWeek (Week 2015 4)
-- 2015-W05
-- >>> prevWeek (Week 2015 0)
-- 2014-W52
-- >>> nextWeek (Week 2015 53)
-- 2016-W01
nextWeek,prevWeek :: Week -> Week
nextWeek = weekOfDay . addDays 1 . endOfWeek
prevWeek = weekOfDay . addDays (-1) . startOfWeek

data Month = Month { _monthYear :: Integer , _monthNum :: Int }
makeLenses ''Month

instance Show Month where
  show (Month y m) = show y ++ "-" ++ printf "%02d" m

-- |
-- >>> mkMonth 2015 1
-- Just 2015-01
-- >>> mkMonth 2015 12
-- Just 2015-12
-- >>> mkMonth 2015 0
-- Nothing
-- >>> mkMonth 2015 13
-- Nothing
mkMonth :: Integer -> Int -> Maybe Month
mkMonth y m = Month <$> pure y <*> mfilter (between 1 12) (Just m)

-- |
-- >>> parseMonth "2015-01"
-- Just 2015-01
-- >>> parseMonth "2015-1"
-- Just 2015-01
-- >>> parseMonth "2015-12"
-- Just 2015-12
-- >>> parseMonth "2015-13"
-- Nothing
-- >>> parseMonth "2015-W12"
-- Nothing
parseMonth :: String -> Maybe Month
parseMonth s = join $ mkMonth <$> y <*  sep <*>  m
  where
    y = readMay . take 4 $ s
    sep  = mfilter (== "-") . Just . take 1 . drop 4 $ s
    m = readMay . drop 5 $ s

parseHumanMonth :: String -> IO (Maybe Month)
parseHumanMonth "this_month" = Just . monthOfDay <$> getCurrentDay
parseHumanMonth "last_month" = Just . prevMonth . monthOfDay <$> getCurrentDay
parseHumanMonth s            = pure $ parseMonth s

-- |
-- >>> monthOfDay (fromGregorian 2015 01 25)
-- 2015-01
monthOfDay :: Day -> Month
monthOfDay = Month
  <$> view years
  <*> view months

-- |
-- >>> startOfMonth (Month 2015 1)
-- 2015-01-01
-- >>> endOfMonth (Month 2015 1)
-- 2015-01-31
-- >>> endOfMonth (Month 2015 4)
-- 2015-04-30
-- >>> endOfMonth (Month 2015 2)
-- 2015-02-28
-- >>> endOfMonth (Month 2012 2)
-- 2012-02-29
startOfMonth,endOfMonth :: Month -> Day
startOfMonth (Month y m) = fromGregorian y m 1
endOfMonth   (Month y m) = fromGregorian y m (monthLength (isLeapYear y) m)

-- |
-- >>> prevMonth (Month 2015 2)
-- 2015-01
-- >>> nextMonth (Month 2015 2)
-- 2015-03
-- >>> prevMonth (Month 2015 1)
-- 2014-12
-- >>> nextMonth (Month 2015 12)
-- 2016-01
nextMonth,prevMonth :: Month -> Month
nextMonth = monthOfDay . addDays 1 . endOfMonth
prevMonth = monthOfDay . addDays (-1) . startOfMonth

data Period
  = ForMonth Month
  | ForWeek Week
  | ForDay Day
makePrisms ''Period

parseHumanPeriod :: String -> IO (Maybe Period)
parseHumanPeriod s = do
  mMay <- fmap ForMonth <$> parseHumanMonth s
  wMay <- fmap ForWeek  <$> parseHumanWeek s
  dMay <- fmap ForDay   <$> parseHumanDay s
  pure $ mMay <|> wMay <|> dMay
