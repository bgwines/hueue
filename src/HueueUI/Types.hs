{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module HueueUI.Types
( HueueUI(..)
, OAuthKeys(..)
) where

import Yesod

import Aliases
import MonadImports

import qualified Utils as U

import qualified Data.Aeson as A

import Control.Exception

import qualified Data.List as List

import qualified Data.Convertible as C

import qualified Data.Text.Internal as T
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSChar8
import qualified Data.ByteString.Internal as BS

import qualified Network (withSocketsDo)
import qualified Network.CGI (formDecode)
import qualified Network.HTTP.Conduit as HTTP

import qualified Token
import qualified Job

import qualified GithubWebhook.Types.BigUser as User

import Database.Persist
import Database.Persist.TH
import Database.Persist.Sqlite

data OAuthKeys = OAuthKeys
    { oauthKeysClientID :: String
    , oauthKeysClientSecret :: String
    }

data HueueUI = HueueUI ConnectionPool OAuthKeys

mkYesod "HueueUI" [parseRoutes|
/ HomeR GET
/oauthRedirect OAuthRedirectR GET
/receiveAccessToken ReceiveAccessTokenR GET
|]
instance Yesod HueueUI

instance YesodPersist HueueUI where
    type YesodPersistBackend HueueUI = SqlBackend

    runDB :: YesodDB HueueUI a -> HandlerT HueueUI IO a
    runDB action = do
        HueueUI pool _ <- getYesod
        runSqlPool action pool

getHomeR :: HandlerT HueueUI IO Html
getHomeR = defaultLayout $ do
    setTitle "Hueue dashboard"
    toWidgetHead [hamlet|<h1>Hueue admin dashboard :o|]
    queue <- handlerToWidget . runDB $ selectList [Job.JobRepoID ==. 61999075] []
    mtoken <- handlerToWidget . runDB $ getBy (Token.UniqueUserID 2442246)
    toWidget
        [hamlet|
            <h2>Jobs:
            $if null queue
                <p>You don't have any jobs.
            $else
                <ul>
                    $forall job <- queue
                        <li>#{show job}
            <h2>Token:
            $case mtoken
                $of Nothing
                    <p>Error when loading token
                $of Just token
                    <p>#{show token}
        |]
    HueueUI _ keys <- getYesod
    let oauthURL = "https://github.com/login/oauth/authorize"
            ++ "?client_id="    ++ oauthKeysClientID keys
            ++ "&redirect_uri=" ++ "http://52.42.19.45:3000/oauthRedirect" -- TODO: Yesod this up
            ++ "&scope="        ++ "repo"
            ++ "&state="        ++ "142857" -- TODO
            ++ "&allow_signup=" ++ "true" :: String
    toWidget
        [hamlet|
            <p><a href=#{oauthURL}>Give Hueue access
        |]

getReceiveAccessTokenR :: HandlerT HueueUI IO Html
getReceiveAccessTokenR = defaultLayout $ do
    maybeAccessToken <- lookupGetParam "access_token"
    _maybeScope <- lookupGetParam "scope"
    _maybeTokenType <- lookupGetParam "token_type"
    toWidget
        [hamlet|
            $maybe accessToken <- maybeAccessToken
                <h1>"FINAL STAGE??1!!!?!!"
                <h2>access token
                <p>#{accessToken}
            $nothing
                <h1>Fatal: not given access stoken
            |]

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
    . try
    . Network.withSocketsDo
    $ do
        req <- (addHeaders . specifyMethod . HTTP.urlEncodedBody params) <$> HTTP.parseUrl url
        HTTP.newManager HTTP.tlsManagerSettings >>= HTTP.httpLbs req
    where
        displayException' :: SomeException -> String
        displayException' = displayException

getOAuthRedirectR :: HandlerT HueueUI IO Html
getOAuthRedirectR = defaultLayout $ do
    maybeCodeState <- liftA2 (,) <$> lookupGetParam "code" <*> lookupGetParam "state"
    HueueUI _ keys <- getYesod
    eitherBlockResult <- liftIO . runEitherT $ do
        let extractionErrorMsg = "Fatal: couldn't extract code and state" :: String
        (accessTokenRequestCode, state) <- hoistEither . U.note extractionErrorMsg $ maybeCodeState

        unless (state == "142857") $
            left "Fatal: security attack detected; aborting authentication"

        let url = "https://github.com/login/oauth/access_token"
        let params = [ ("client_id"    , (BSChar8.pack $ oauthKeysClientID keys))
                     , ("client_secret", (BSChar8.pack $ oauthKeysClientSecret keys))
                     , ("code"         , (C.convert accessTokenRequestCode))
                     , ("redirect_uri" , "http://52.42.19.45:3000/receiveAccessToken")
                     , ("state"        , "142857")
                     ]

        resultBody <- HTTP.responseBody <$> sendHTTPRequest url params id methodPost
        let decodedResultBody = Network.CGI.formDecode . BSChar8.unpack . L.toStrict $ resultBody
        accessToken <- hoistEither . U.note "Fatal: couldn't extract access token" $ snd <$> List.find ((==) "access_token" . fst) decodedResultBody

        let addHeaders r = r { HTTP.requestHeaders = [("User-Agent", "hueue"), ("Authorization", BSChar8.pack ("token " ++ accessToken))]}
        userJSON <- HTTP.responseBody <$> sendHTTPRequest "https://api.github.com/user" [] addHeaders methodGet
        user <- (hoistEither . A.eitherDecode $ userJSON) :: EIO User.BigUser
        right (user, accessToken)
    case eitherBlockResult of
        Left errorMsg ->
            toWidget
                [hamlet|
                    <p>#{show errorMsg}
                    |]

        Right (user, accessToken) -> do
            let userIDInt = fromIntegral $ User.id user
            handlerToWidget . runDB $ insert (Token.OAuth2Token userIDInt accessToken)
            eitherWrittenToken <- handlerToWidget . runDB $ getBy (Token.UniqueUserID userIDInt)

            toWidget
                [hamlet|
                    <p>#{show user}
                    <p>#{show eitherWrittenToken}
                    |]
