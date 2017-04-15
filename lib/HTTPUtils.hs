{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}

module HTTPUtils
( sendHTTPRequest
, methodGet
, methodPost
) where

import Aliases
import MonadImports

import qualified Control.Exception as E

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Internal as BS

import qualified Network (withSocketsDo)
import qualified Network.CGI (formDecode)
import qualified Network.HTTP.Conduit as HTTP

type RequestModifier = HTTP.Request -> HTTP.Request

methodPost :: RequestModifier
methodPost r = r { HTTP.method = "POST" }

methodGet :: RequestModifier
methodGet r = r { HTTP.method = "GET" }

sendHTTPRequest
    :: String
    -> [(BS.ByteString, BS.ByteString)]
    -> RequestModifier
    -> RequestModifier
    -> EIO (HTTP.Response L.ByteString)
sendHTTPRequest url params addHeaders specifyMethod
    = bimapEitherT displayException' id
    . EitherT
    . liftIO
    . E.try
    . Network.withSocketsDo
    $ do
        req <- (addHeaders . specifyMethod . HTTP.urlEncodedBody params) <$> HTTP.parseUrl url
        HTTP.newManager HTTP.tlsManagerSettings >>= HTTP.httpLbs req
    where
        displayException' :: E.SomeException -> String
        displayException' = E.displayException
