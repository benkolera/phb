{-# LANGUAGE FlexibleContexts #-}
module Phb.Util where

import BasePrelude
import Prelude     ()

import Control.Error (hush)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist (ToBackendKey,Key,PersistEntity,keyFromValues,toPersistValue)
import Database.Persist.Sql (fromSqlKey,SqlBackend)
import Data.Time

getCurrentDay :: IO Day
getCurrentDay = utctDay <$> getCurrentTime

fromGregorianTime :: Integer -> Int -> Int -> Int -> Int -> Pico -> LocalTime
fromGregorianTime y m d hh mm ss =
  LocalTime (fromGregorian y m d) (TimeOfDay hh mm ss)

keyToText :: ToBackendKey SqlBackend a => Key a -> Text
keyToText = T.pack . show . fromSqlKey

textToKey :: PersistEntity r => Text -> Maybe (Key r)
textToKey = stringToKey . T.unpack

stringToKey :: PersistEntity r => String -> Maybe (Key r)
stringToKey s = do
  idInt <- readMaybe s :: Maybe Int64
  hush . keyFromValues $ [toPersistValue idInt]
