{-# LANGUAGE OverloadedStrings #-}

module Site
  ( app
  ) where

import BasePrelude hiding (Handler, app)
import Prelude     ()

import           Control.Lens
import           Database.Persist.Sql                        (runMigration)
import           Heist
import qualified Heist.Interpreted                           as I
import           Snap
import           Snap.Snaplet.Auth                           (addAuthSplices)
import           Snap.Snaplet.Heist.Compiled
import           Snap.Snaplet.Persistent
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe

import Phb.Auth          (initPhbAuthManager)
import Phb.Db            (migrateAll)
import Phb.Ldap          (mkLdapConfig)
import Phb.Mail          (mkMailConfig)
import Site.Action
import Site.Auth
import Site.Backlog
import Site.Customer
import Site.Event
import Site.Heartbeat
import Site.Internal
import Site.Person
import Site.Project
import Site.Standup
import Site.Task
import Site.TimeLog
import Site.WorkCategory

routes :: PhbRoutes
routes = fold $
  [ actionRoutes
  , backlogRoutes
  , customerRoutes
  , eventRoutes
  , heartbeatRoutes
  , authRoutes
  , personRoutes
  , projectRoutes
  , standupRoutes
  , taskRoutes
  , timeLogRoutes
  , workCategoryRoutes
  , [ ("",ifTop $ render "index")
    , ("",serveDirectory "static")
    ]
  ]

app :: SnapletInit Phb Phb
app = makeSnaplet "Phb" "A pointy haired boss" Nothing $ do
  p <- nestSnaplet "" db $ initPersist (runMigration migrateAll)
  h <- nestSnaplet "" heist $ heistInit' "templates" phbHeistConfig
  s <- nestSnaplet "sess" sess $
    initCookieSessionManager "site_key.txt" "sess" (Just 3600)
  a <- nestSnaplet "auth" auth $
    initPhbAuthManager sess (persistPool . view snapletValue $ p)
  c <- getSnapletUserConfig
  m <- liftIO $ mkMailConfig c
  l <- liftIO $ mkLdapConfig c
  addAuthSplices h auth
  addRoutes routes
  return $ Phb h p a s m l

allCompiledSplices :: Splices PhbSplice
allCompiledSplices = fold
  [ allActionSplices
  , allBacklogSplices
  , allCustomerSplices
  , allEventSplices
  , allHeartbeatSplices
  , allAuthSplices
  , allPersonSplices
  , allProjectSplices
  , allStandupSplices
  , allTaskSplices
  , allTimeLogSplices
  , allWorkCategorySplices
  , flashSplices
  ]

defaultTitle :: I.Splice IO
defaultTitle = I.textSplice "A high level team visibility tool."

allLoadTimeSplices :: Splices (I.Splice IO)
allLoadTimeSplices = defaultLoadTimeSplices <> do
  "pageTitle" ## defaultTitle

phbHeistConfig :: HeistConfig PhbHandler
phbHeistConfig =
  emptyHeistConfig
    & hcCompiledSplices .~ allCompiledSplices
    & hcLoadTimeSplices .~ allLoadTimeSplices
    & hcErrorNotBound   .~ True
    & hcNamespace       .~ ""
