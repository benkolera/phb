{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
module Phb.Types.Customer.Create 
  ( CustomerCreate(CustomerCreate)
  , customerCreateName
  ) where

import Control.Lens (makeLensesFor)
import Data.Aeson   (FromJSON)
import GHC.Generics (Generic)
import Data.Text    (Text)

data CustomerCreate = CustomerCreate
  { name :: Text
  } deriving (Generic)
makeLensesFor
  [ ("name","customerCreateName") ]
  ''CustomerCreate

instance FromJSON CustomerCreate
