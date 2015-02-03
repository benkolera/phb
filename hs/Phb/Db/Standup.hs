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

loadStandupForDay = _
