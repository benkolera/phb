{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}

module Phb.Server.Cookie where

import           Data.Typeable               (Proxy(Proxy))
import           Data.String                 (fromString)
import           GHC.TypeLits                (KnownSymbol, Symbol, symbolVal)
import           Network.Wai                 (requestHeaders)
import           Servant.API                 ((:>))
import           Servant.Server              (HasServer(ServerT,route))
import           Servant.Common.Text         (FromText, fromText)
import           Web.Cookie                  (parseCookiesText)

data Cookie (sym :: Symbol) a = Cookie 

instance (KnownSymbol sym, FromText a, HasServer sublayout)
  => HasServer (Cookie sym a :> sublayout) where

  type ServerT (Cookie sym a :> sublayout) m =
    Maybe a -> ServerT sublayout m

  route Proxy subserver request respond = do
    let mheader = lookup "cookie" (requestHeaders request)
        mc = fromText =<< lookup str =<< fmap parseCookiesText mheader
    route (Proxy :: Proxy sublayout) (subserver mc) request respond
      where str = fromString $ symbolVal (Proxy :: Proxy sym)
