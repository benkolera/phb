{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module Phb.Types.User
  ( User(User)
  , userUsername
  , userId
  ) where

import Control.Lens (makeLensesFor)
import GHC.Generics (Generic)
import Data.Text (Text)

data User = User
  { id       :: Int
  , username :: Text
  } deriving (Generic)
makeLensesFor
  [ ("username","userUsername")
  , ("id","userId") ]
  ''User
