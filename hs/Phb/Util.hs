{-# LANGUAGE FlexibleContexts #-}
module Phb.Util where

import BasePrelude
import Prelude     ()

import Control.Error (hush)
import Data.Text (Text)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NEL
import qualified Data.Text as T
import Database.Persist (ToBackendKey,Key,PersistEntity,keyFromValues,toPersistValue)
import Database.Persist.Sql (fromSqlKey,SqlBackend)

keyToText :: ToBackendKey SqlBackend a => Key a -> Text
keyToText = T.pack . show . fromSqlKey

textToKey :: PersistEntity r => Text -> Maybe (Key r)
textToKey = stringToKey . T.unpack

stringToKey :: PersistEntity r => String -> Maybe (Key r)
stringToKey s = do
  idInt <- readMaybe s :: Maybe Int64
  hush . keyFromValues $ [toPersistValue idInt]

partitionOn :: Ord b => (a -> b) -> [a] -> [NonEmpty a]
partitionOn f = NEL.groupBy (on (==) f) . sortBy (on compare f)
