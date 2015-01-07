{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Phb.Types.Success where

import BasePrelude
import Prelude     ()

import Control.Lens
import Data.Text        (Text)
import Database.Persist (Entity, Key)

import qualified Phb.Db.Internal as D

data Success = Success
  { _successKey          :: Key D.Success
  , _successWho          :: [Entity D.Person]
  , _successWhat         :: Text
  , _successAchievements :: [Text]
  } deriving Show
makeLenses ''Success
