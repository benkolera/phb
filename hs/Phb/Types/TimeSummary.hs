{-# LANGUAGE TemplateHaskell #-}
module Phb.Types.TimeSummary where

import Control.Lens
import Data.Text        (Text)
import Database.Persist (Entity)

import qualified Phb.Db.Internal as D

data TimeSummary = TimeSummary
  { _timeSummaryLabel  :: Text
  , _timeSummaryHours  :: Double
  , _timeSummaryPeople :: [Entity D.Person]
  }
makeLenses ''TimeSummary
