{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Phb.Types.Record where

import           Control.Lens  (makeLenses)
import           Control.Monad (mzero)
import           Data.Aeson    (ToJSON(toJSON),object,(.=),(.:),FromJSON(parseJSON),Value(Object))
import           Data.Proxy    (Proxy(Proxy))
import           Data.Text     (pack)
import           GHC.TypeLits  (Symbol,KnownSymbol,symbolVal)

data RecordList ( sym :: Symbol ) a = RecordList
  { _records :: [a] }
makeLenses ''RecordList

instance (ToJSON a , KnownSymbol sym) => ToJSON (RecordList sym a) where
  toJSON (RecordList rs) = object [ keyName .= toJSON rs ]
    where keyName = pack . symbolVal $ (Proxy :: Proxy sym)

data Record (sym :: Symbol) a = Record
  { _record :: a }
makeLenses ''Record

instance (ToJSON a , KnownSymbol sym) => ToJSON (Record sym a) where
  toJSON (Record r) = object [ keyName .= toJSON r ]
    where keyName = pack . symbolVal $ (Proxy :: Proxy sym)

instance (FromJSON a , KnownSymbol sym) => FromJSON (Record sym a) where
  parseJSON (Object o) = o .: keyName
    where keyName = pack . symbolVal $ (Proxy :: Proxy sym)
  parseJSON _ = mzero
