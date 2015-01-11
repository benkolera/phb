{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Phb.Ldap
  ( loginLdap
  , LdapConfig(..)
  , LdapAuthError (..)
  , mkLdapConfig
  ) where

import           BasePrelude             hiding (first, left)
import           Prelude                 ()

import           Control.Error
import           Control.Lens
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Control.Monad.Reader    (MonadReader, asks)
import           Control.Monad.Trans     (lift)
import           Data.Bifunctor          (first)
import qualified Data.Configurator       as C
import qualified Data.Configurator.Types as C
import           Data.Text               (Text, unpack)
import           Data.Text.Lens
import           LDAP                    hiding (ldapPort)

data LdapConfig = LdapConfig
  { _ldapHost   :: Text
  , _ldapPort   :: Int
  , _ldapBaseDn :: Maybe Text
  , _ldapScope  :: LDAPScope
  }
makeLenses ''LdapConfig

mkLdapConfig :: C.Config -> IO LdapConfig
mkLdapConfig c = LdapConfig
  <$> C.require c "ldap.hostname"
  <*> C.lookupDefault 389 c "ldap.port"
  <*> C.lookup c "ldap.baseDn"
  <*> pure LdapScopeSubtree

data LdapAuthError = ConnectException LDAPException | NoSuchUser | WrongPassword deriving Show

loginLdap :: (MonadReader LdapConfig m, MonadIO m,Functor m) => Text -> Text -> EitherT LdapAuthError m LDAPEntry
loginLdap user pass = do
  baseDn <- lift $ asks (^?ldapBaseDn._Just.unpacked)
  scope  <- lift $ view ldapScope
  host   <- lift $ view (ldapHost.unpacked)
  port   <- lift $ view (ldapPort.to fromIntegral)
  ldap   <- tryLdap $ ldapInit host port
  es     <- tryLdap $ ldapSearch ldap baseDn scope userQuery LDAPAllUserAttrs False
  maybe (left NoSuchUser) (userBind ldap) $ headMay es

  where
    userBind ldap e@(LDAPEntry dn _) =
      bimapEitherT (const WrongPassword) (const e) $
        tryLdap (ldapSimpleBind ldap dn (unpack pass))
    tryLdap a =
      EitherT . liftIO $ first ConnectException <$> try a
    userQuery = Just ("uid=" <> (unpack user))
