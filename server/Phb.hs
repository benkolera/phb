{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Control.Applicative                  ((<$>),(<*>))
import Database.PostgreSQL.Simple           (connectPostgreSQL)
import Servant                              (serve)
import Network.Wai                          (Application)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Network.Wai.Handler.Warp             (run)
import Web.ClientSession                    (getDefaultKey)

import Phb.Db     (DbEnv(DbEnv))
import Phb.Api    (ApiEnv(ApiEnv))
import Phb.Server

app :: PhbEnv -> Application
app e = logStdout (serve phbAPI (server e))

main :: IO ()
main = do
  -- Dirty dirty for now
  e <- PhbEnv
    <$> getDefaultKey
    <*> (ApiEnv . DbEnv <$> connectPostgreSQL "dbname='phb2'")
  run 8000 (app e)
