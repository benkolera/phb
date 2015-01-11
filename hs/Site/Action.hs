{-# LANGUAGE OverloadedStrings #-}
module Site.Action where

import BasePrelude
import Prelude     ()

import Heist
import Snap                        (ifTop)
import Snap.Snaplet.Heist.Compiled

import Site.Internal

actionRoutes :: PhbRoutes
actionRoutes =
  [("/actions",ifTop $ render "actions/all")]

allActionSplices :: Splices PhbSplice
allActionSplices = mempty
