{-# OPTIONS_GHC -fno-warn-orphans  #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Snap.Snaplet.Persistent
  ( initPersist
  , PersistState(..)
  , HasPersistPool(..)
  , mkPgPool
  , mkSnapletPgPool
  , runPersist
  , withPool
  ) where

-------------------------------------------------------------------------------
import Control.Monad.Catch          as EC
import Control.Monad.Logger
import Control.Monad.State
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Resource
import Control.Retry
import Data.Configurator
import Data.Configurator.Types
import Database.Persist.Postgresql  hiding (get)
import Snap.Core
import Snap.Snaplet                 as S
-------------------------------------------------------------------------------

instance MonadThrow Snap where
    throwM = liftSnap . throwM

instance MonadCatch Snap where
    catch e h = liftSnap $ catch e h

-------------------------------------------------------------------------------
newtype PersistState = PersistState { persistPool :: ConnectionPool }


-------------------------------------------------------------------------------
-- | Implement this type class to have any monad work with snaplet-persistent.
-- A default instance is provided for (Handler b PersistState).
class MonadIO m => HasPersistPool m where
    getPersistPool :: m ConnectionPool

instance HasPersistPool (S.Handler b PersistState) where
    getPersistPool = gets persistPool

instance MonadIO m => HasPersistPool (ReaderT ConnectionPool m) where
    getPersistPool = ask


-------------------------------------------------------------------------------
-- | Initialize Persistent with an initial SQL function called right
-- after the connection pool has been created. This is most useful for
-- calling migrations upfront right after initialization.
--
-- Example:
--
-- > initPersist (runMigrationUnsafe migrateAll)
--
-- where migrateAll is the migration function that was auto-generated
-- by the QQ statement in your persistent schema definition in the
-- call to 'mkMigrate'.
initPersist :: SqlPersistT (LoggingT IO) a -> SnapletInit b PersistState
initPersist migration = makeSnaplet "persist" description datadir $ do
    conf <- getSnapletUserConfig
    p <- liftIO . runStderrLoggingT $ mkSnapletPgPool conf

    void . liftIO . runStderrLoggingT $ runSqlPool migration p
    return $ PersistState p
  where
    description = "Snaplet for persistent DB library"
    datadir = Nothing

-------------------------------------------------------------------------------
-- | Constructs a connection pool from Config.
mkPgPool :: (MonadLogger m, MonadBaseControl IO m, MonadIO m) => Config -> m ConnectionPool
mkPgPool conf = do
  pgConStr <- liftIO $ require conf "postgre-con-str"
  cons <- liftIO $ require conf "postgre-pool-size"
  createPostgresqlPool pgConStr cons

-------------------------------------------------------------------------------
-- | Constructs a connection pool in a snaplet context.
mkSnapletPgPool :: (MonadBaseControl IO m, MonadLogger m, MonadIO m, EC.MonadCatch m) => Config -> m ConnectionPool
mkSnapletPgPool = mkPgPool

-------------------------------------------------------------------------------
-- | Runs a SqlPersist action in any monad with a HasPersistPool instance.
runPersist :: (HasPersistPool m, MonadSnap m)
           => SqlPersistT (ResourceT (LoggingT IO)) b
           -- ^ Run given Persistent action in the defined monad.
           -> m b
runPersist action = do
  pool <- getPersistPool
  withPool pool action

------------------------------------------------------------------------------
-- | Run a database action, if a `PersistentSqlException` is raised
-- the action will be retried four times with a 50ms delay between
-- each retry.
--
-- This is being done because sometimes Postgres will reap connections
-- and the connection leased out of the pool may then be stale and
-- will often times throw a `Couldn'tGetSQLConnection` type value.
withPool :: (MonadIO m)
         => ConnectionPool
         -> SqlPersistT (ResourceT (LoggingT IO)) a
         -> m a
withPool cp f = liftIO $ recoverAll retryPolicy (runF f cp)
  where
    retryPolicy = constantDelay 50000 <> limitRetries 5
    runF f' cp' = liftIO . runStderrLoggingT . runResourceT $ runSqlPool f' cp'
