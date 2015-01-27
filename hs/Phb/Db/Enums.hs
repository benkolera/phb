{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Phb.Db.Enums
  ( BacklogStatusEnum(..)
  , EventStatusEnum(..)
  , StatusColourEnum(..)
  ) where

import           Control.Lens
import           Database.Persist.TH (derivePersistField)

data BacklogStatusEnum
  = BacklogScoped
  | BacklogNeedsScoping
  | BacklogInCommercials
  | BacklogProjectStarted
  | BacklogRejected
  deriving (Show,Read,Eq,Enum,Bounded)

makePrisms ''BacklogStatusEnum
data StatusColourEnum
  = StatusGreen
  | StatusAmber
  | StatusRed
  | StatusGray
  deriving (Show,Read,Eq,Enum,Bounded)
makePrisms ''StatusColourEnum

data EventStatusEnum
  = EventSlaMet
  | EventSlaJustMet
  | EventSlaNotMet
  deriving (Show,Read,Eq,Enum,Bounded)
makePrisms ''EventStatusEnum

derivePersistField "BacklogStatusEnum"
derivePersistField "EventStatusEnum"
derivePersistField "StatusColourEnum"
