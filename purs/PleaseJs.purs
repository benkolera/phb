module PleaseJs where

import Control.Monad.Eff

foreign import data PleaseJs :: !

foreign import make_color
  "Please.make_color()[0];" :: forall eff. Eff (pleaseJs :: PleaseJs | eff ) String
