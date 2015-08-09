{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
module Phb.Types.User
  ( User(User)
  , userUsername
  , userId
  ) where

import Control.Lens (makeLensesFor)
import Data.Aeson   (FromJSON,ToJSON)
import GHC.Generics (Generic)
import Data.Text    (Text)

data User = User
  { id       :: Int
  , username :: Text
  } deriving (Generic)
makeLensesFor
  [ ("username","userUsername")
  , ("id","userId") ]
  ''User

instance FromJSON User
instance ToJSON   User
