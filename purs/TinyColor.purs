module TinyColor where

foreign import data TinyColor :: *

foreign import tinycolor :: String -> TinyColor

foreign import lighten
  """
  function lighten(tc) {
    return tc.lighten();
  }
  """ :: TinyColor -> TinyColor

foreign import toHex
  """
  function toHex(tc) {
    return tc.toHex();
  }
  """ :: TinyColor -> String
