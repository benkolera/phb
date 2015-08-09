{-# LANGUAGE TemplateHaskell #-}
module Phb.Server.Env
  ( PhbEnv(PhbEnv)
  , AuthedPhbEnv(AuthedPhbEnv)
  , phbSessionKey
  , phbApiEnv
  , authedPhbUser
  ) where

import Control.Lens      (makeClassy)
import Web.ClientSession (Key)

import Phb.Types.User   (User)
import Phb.Db           (HasDbEnv(dbEnv))
import Phb.Api.Internal (ApiEnv,HasApiEnv(apiEnv))

data PhbEnv = PhbEnv
  { _phbSessionKey :: Key
  , _phbApiEnv     :: ApiEnv
  }
makeClassy ''PhbEnv

instance HasApiEnv PhbEnv where
  apiEnv = phbApiEnv . apiEnv

instance HasDbEnv PhbEnv where
  dbEnv = phbApiEnv . dbEnv

data AuthedPhbEnv = AuthedPhbEnv
  { _authedPhbUser   :: User
  , _authedPhbPhbEnv :: PhbEnv
  }
makeClassy ''AuthedPhbEnv

instance HasApiEnv AuthedPhbEnv where
  apiEnv = authedPhbEnv . apiEnv

instance HasDbEnv AuthedPhbEnv where
  dbEnv = authedPhbPhbEnv . dbEnv

instance HasPhbEnv AuthedPhbEnv where
  phbEnv = authedPhbPhbEnv . phbEnv
