{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module Phb.Types.Login 
  ( Login(Login)
  , loginUsername
  , loginPassword
  ) where

import Control.Lens (makeLensesFor)
import GHC.Generics (Generic)
import Data.Text (Text)

data Login = Login
  { username :: Text
  , password :: Text
  } deriving (Generic)
makeLensesFor
  [ ("username","loginUsername")
  , ("password","loginPassword") ]
  ''Login
