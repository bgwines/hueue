{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module HueueUI.Types
( HueueUI(..)
) where

import Yesod

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

import qualified QueueStore.Store
import qualified QueueStore.Types.JobQueue

import qualified GithubWebhook.Types.BigUser as User

data HueueUI = HueueUI

mkYesod "HueueUI" [parseRoutes|
/ HomeR GET
/clearQueue ClearQueueR GET
/oauthRedirect OAuthRedirectR GET
/receiveAccessToken ReceiveAccessTokenR GET
|]
instance Yesod HueueUI

prettyPrintQueue :: QueueStore.Types.JobQueue.JobQueue -> String
prettyPrintQueue = show

getHomeR :: HandlerT HueueUI IO Html
getHomeR = defaultLayout $ do
    setTitle "Hueue dashboard"
    toWidgetHead [hamlet|<h1>Hueue dashboard (powered by HueueUI! :o)|]
    addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.6.2/jquery.min.js"
    toWidget
        [julius|
            $(function() {
                $("h1").click(function(){
                    alert("You clicked on the heading!");
                });
            });
        |]
    equeue <- liftIO . runEitherT $ QueueStore.Store.loadQueueDEBUG 61999075
    toWidget
        [hamlet|
            <h2>Jobs:
            $case equeue
                $of Left msg
                    <p>Error when loading jobs; failed with error message #{msg}
                $of Right queue
                    $if QueueStore.Types.JobQueue.null queue
                        <p>You don't have any jobs.
                    $else
                        <ul>
                            $forall job <- QueueStore.Types.JobQueue.queue queue
                                <li>#{show job}
        |]
    let oauthURL = "https://github.com/login/oauth/authorize"
            ++ "?client_id="    ++ "416fdf5ed5fb66f16bd3" -- TODO: DB this up
            ++ "&redirect_uri=" ++ "http://52.43.33.20:3000/oauthRedirect" -- TODO: Yesod this up
            ++ "&scope="        ++ "repo"
            ++ "&state="        ++ "142857" -- TODO
            ++ "&allow_signup=" ++ "true" :: String
    toWidget
        [hamlet|
            <p><a href=#{oauthURL}>Give Hueue access
        |]

getClearQueueR :: HandlerT HueueUI IO Html
getClearQueueR = defaultLayout $ do
    clearResult <- liftIO . runEitherT $ QueueStore.Store.clearQueueDEBUG 61999075
    toWidget
        [hamlet|
            <h2>Clearing the repo's jobqueue
            $case clearResult
                $of Left msg
                    <p>Error when clearing queue; failed with error message #{msg}
                $of Right ()
                    <p>Cleared successfully!
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

getAccessTokenPOSTParams :: BS.ByteString -> [(BS.ByteString, BS.ByteString)]
getAccessTokenPOSTParams code =
    [ ("client_id"    , "416fdf5ed5fb66f16bd3") -- TODO: DB this up
    , ("client_secret", "298f94844d493cc1deccf97ba54a268d1b5690a8") -- TODO
    , ("code"         , code)
    , ("redirect_uri" , "http://52.43.33.20:3000/receiveAccessToken") -- TODO: Yesod this up
    , ("state"        , "142857") -- TODO (T.showText code)
    ]

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
    -> EitherT SomeException IO (HTTP.Response L.ByteString)
sendHTTPRequest url params addHeaders specifyMethod = EitherT . try $ Network.withSocketsDo $ do
    req <- (addHeaders . specifyMethod . HTTP.urlEncodedBody params) <$> HTTP.parseUrl url
    HTTP.newManager HTTP.tlsManagerSettings >>= HTTP.httpLbs req

-- for debugging only
getHTTPRequest
    :: String
    -> [(BS.ByteString, BS.ByteString)]
    -> RequestModifier
    -> RequestModifier
    -> IO (HTTP.Request)
getHTTPRequest url params addHeaders specifyMethod = Network.withSocketsDo $ do
    (specifyMethod . addHeaders . HTTP.urlEncodedBody params) <$> HTTP.parseUrl url

-- TODO: move this somewhere
instance Exception String

maybeToEitherT :: (Monad m) => String -> Maybe a -> EitherT SomeException m a
maybeToEitherT msg = U.amplifyError . hoistEither . U.note msg

getOAuthRedirectR :: HandlerT HueueUI IO Html
getOAuthRedirectR = defaultLayout $ do
    maybeCodeState <- liftA2 (,) <$> lookupGetParam "code" <*> lookupGetParam "state"
    eitherBlockResult <- liftIO . runEitherT $ do
        let extractionErrorMsg = "Fatal: couldn't extract code and state" :: String
        (accessTokenRequestCode, state) <- maybeToEitherT extractionErrorMsg $ maybeCodeState

        let incorrectStateMessage = "Fatal: security attack detected; aborting authentication" :: String
        unless (state == "142857") $
            left (toException incorrectStateMessage)

        let url = "https://github.com/login/oauth/access_token"
        let params = getAccessTokenPOSTParams (C.convert accessTokenRequestCode)
        resultBody <- HTTP.responseBody <$> sendHTTPRequest url params id methodPost
        let decodedResultBody = Network.CGI.formDecode . BSChar8.unpack . L.toStrict $ resultBody
        accessToken <- maybeToEitherT "Fatal: couldn't extract access token" $ snd <$> List.find ((==) "access_token" . fst) decodedResultBody

        --TokenStore.writeToken accessToken

        let addHeaders r = r { HTTP.requestHeaders = [("User-Agent", "hueue"), ("Authorization", BSChar8.pack ("token " ++ accessToken))]}
        userJSON <- HTTP.responseBody <$> sendHTTPRequest "https://api.github.com/user" [] addHeaders methodGet
        user <- (U.amplifyError . hoistEither . A.eitherDecode $ userJSON) :: EitherT SomeException IO User.BigUser
        right user
    case eitherBlockResult of
        Left someException ->
            toWidget
                [hamlet|
                    <p>#{show someException}
                    |]

        Right user ->
            toWidget
                [hamlet|
                    <p>#{show user}
                    |]
