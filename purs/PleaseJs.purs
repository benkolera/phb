module PleaseJs where

import Control.Monad.Eff

foreign import data PleaseJs :: !

foreign import makeColor
  """function makeColor() {
    return Please.make_color()[0];
  };""":: forall eff. Eff (pleaseJs :: PleaseJs | eff ) String
