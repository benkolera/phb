module Phb.Main where

import BasePrelude
import Prelude     ()

import qualified Data.ByteString.Lazy          as LBS
import           Database.Persist.Sql          (toSqlKey)
import           Text.Blaze.Html.Renderer.Utf8 (renderHtml)

import Phb.Db.Heartbeat (loadHeartbeat)
import Phb.Db.Setup     (setup)
import Phb.Output.Email (heartbeatHtml)

printHtml :: IO ()
printHtml = do
  --setup
  hMay <- loadHeartbeat (toSqlKey 1)
  traverse_ (LBS.writeFile "heartbeat.html" . renderHtml . heartbeatHtml) hMay
