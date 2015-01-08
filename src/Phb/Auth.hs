{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Phb.Auth
    ( PhbAuthManager
    , initPhbAuthManager
    , loginViaLdap
    , userDbKey
    ) where

import           BasePrelude                  hiding (Handler, left)
import           Prelude                      ()

import           Control.Error
import           Control.Lens
import           Control.Monad.Logger         (NoLoggingT)
import           Control.Monad.Reader         (runReaderT)
import           Control.Monad.Trans
import           Control.Monad.Trans.Resource (ResourceT)
import qualified Data.HashMap.Strict          as HM
import           Data.Text                    (Text)
import qualified Data.Text.Encoding           as T
import           Database.Esqueleto           hiding ((^.))
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Persistent
import           Snap.Snaplet.Session
import           Web.ClientSession            (getKey)

import           Phb.Db                       hiding (lookupByLogin)
import qualified Phb.Db                       as D
import           Phb.Ldap
import           Phb.Util

db2Au :: WholeLogin -> AuthUser
db2Au (Entity pk p,Entity _ pl) = AuthUser
  { userId               = Just . UserId . keyToText $ pk
  , userLogin            = pl^.personLoginLogin
  , userEmail            = p^.personEmail.to Just
  , userPassword         = pl^.personLoginPassword.to (Just . ClearText . T.encodeUtf8)
  , userActivatedAt      = pl^.personLoginActivatedAt
  , userRememberToken    = pl^.personLoginRememberToken
  , userSuspendedAt      = pl^.personLoginSuspendedAt
  , userLoginCount       = pl^.personLoginLoginCount
  , userFailedLoginCount = pl^.personLoginFailedLoginCount
  , userLockedOutUntil   = pl^.personLoginLockedOutUntil
  , userCurrentLoginAt   = pl^.personLoginCurrentLoginAt
  , userLastLoginAt      = pl^.personLoginLastLoginAt
  , userCurrentLoginIp   = pl^?personLoginCurrentIp._Just.to T.encodeUtf8
  , userLastLoginIp      = pl^?personLoginLastIp._Just.to T.encodeUtf8
  , userCreatedAt        = pl^.personLoginCreatedAt.to Just
  , userUpdatedAt        = pl^.personLoginUpdatedAt.to Just
  , userResetToken       = pl^.personLoginResetToken
  , userResetRequestedAt = pl^.personLoginResetRequestedAt
  , userRoles            = []
  , userMeta             = HM.empty
  }

data PhbAuthManager = PhbAM {
  _phbPool :: ConnectionPool
  }
makeLenses ''PhbAuthManager


loginViaLdap
  :: Lens' b LdapConfig
  -> Text
  -> Text
  -> Bool
  -> Handler b (AuthManager b) (Either AuthFailure AuthUser)
loginViaLdap ll u p _ = runEitherT $ do
  a <- EitherT $ withBackend $ \ am ->
    note UserNotFound <$> (liftIO $ lookupByLogin am u)
  lc <- lift . withTop' id $ view ll
  ldapAuth a lc
  where
    ldapAuth a lc =
      eitherT (authFailure a) (authSuccess a)
      . fmapLT xlateFail
      . EitherT
      . flip runReaderT lc
      . runEitherT
      $ loginLdap (userLogin a) p

    authSuccess a _ = do
      na <- EitherT $ markAuthSuccess a
      EitherT $ forceLogin a
      pure na

    authFailure a _ = (EitherT $ markAuthFail a) >> (left $ IncorrectPassword)

    xlateFail (ConnectException e) = AuthError . show $ e
    xlateFail NoSuchUser           = UserNotFound
    xlateFail WrongPassword        = IncorrectPassword

initPhbAuthManager
 :: SnapletLens b SessionManager
 -> ConnectionPool
 -> SnapletInit b (AuthManager b)
initPhbAuthManager l pool =
   makeSnaplet "auth" "" Nothing $ do
     aus <- authSettingsFromConfig
     key <- liftIO $ getKey (asSiteKey aus)
     rng <- liftIO mkRNG
     return $ AuthManager
       { backend = PhbAM pool
       , session = l
       , activeUser = Nothing
       , minPasswdLen = asMinPasswdLen aus
       , rememberCookieName = asRememberCookieName aus
       , rememberPeriod = asRememberPeriod aus
       , siteKey = key
       , lockout = asLockout aus
       , randomNumberGenerator = rng
       }

userDbKey :: AuthUser -> Maybe PersonId
userDbKey = (textToKey . unUid =<<) . userId

instance IAuthBackend PhbAuthManager where
  save am = withAuthPool am . upsertAuthUser
  lookupByUserId am = db2AuM . withAuthPool am . D.lookupByUserId
  lookupByLogin am = db2AuM . withAuthPool am . D.lookupByLogin
  lookupByRememberToken am = db2AuM . withAuthPool am . D.lookupByRememberToken
  destroy = fail "We don't allow destroying users."

db2AuM :: (Functor a,Functor b) => a (b WholeLogin) -> a (b AuthUser)
db2AuM = fmap (fmap db2Au)

withAuthPool
  :: (MonadIO m)
  => PhbAuthManager
  -> SqlPersistT (ResourceT (NoLoggingT IO)) a
  -> m a
withAuthPool = withPool . view phbPool
