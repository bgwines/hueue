{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Database.Persist hiding (get)
import Database.Persist.TH
import Database.Persist.Sqlite hiding (get)

import Control.Monad.Trans.Reader

import Control.Monad.IO.Class
import Control.Monad.Logger (runStderrLoggingT)

import Control.Exception

import qualified Data.Convertible as C

import qualified Data.Text.Lazy as T

import qualified Data.Text.Internal as TI
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSChar8
import qualified Data.ByteString.Internal as BS

import qualified Network (withSocketsDo)
import qualified Network.CGI (formDecode)
import qualified Network.HTTP.Conduit as HTTP

import qualified Data.List as List

import qualified GithubWebhook.Types.BigUser as User

import qualified Job
import qualified JobStore
import qualified Repo
import qualified RepoStore
import qualified Token
import qualified TokenStore

import qualified Utils as U

import qualified Data.Aeson as A

import Aliases
import MonadImports

import Data.Aeson (toJSON)

import Web.Scotty
import Data.Monoid (mconcat)

{-
    let githubClientID = "416fdf5ed5fb66f16bd3"
    let githubClientSecret = "298f94844d493cc1deccf97ba54a268d1b5690a8"
    let url = "http://34.208.168.142:3000"
-}
-- TODO: share with other `main`
main :: IO ()
main = runStderrLoggingT $ do
    connectionPool <- createSqlitePool "commonPool" 10
    runSqlPool
        (  runMigration Job.migrateAll
        >> runMigration Repo.migrateAll
        >> runMigration Token.migrateAll ) connectionPool
    liftIO $ serve 3000 connectionPool

serve :: Int -> ConnectionPool -> IO ()
serve port connectionPool = scotty port $ do
    get "/" $ liftIO (T.pack <$> readFile "src/HueueUI/index.html") >>= html

    get "/get_jobs" $ do
        eitherJobs <- runEitherT $ do
            repos <- RepoStore.loadByGithubUserID connectionPool 2442246
            concat <$> mapM -- TODO: just take in a repo?
                (JobStore.loadByRepoID connectionPool . (\(Repo.Repo repoID _) -> repoID)) repos
        jobs <- case eitherJobs of
            (Left msg) -> return []
            (Right js) -> return js
        json . map toJSON $ jobs

    get "/oauthRedirect" $ do
        accessTokenRequestCode :: String <- param "code"
        state :: String <- param "state"
        result <- runEitherT $ do
            unless (state == "142857") $
                left "Fatal: security attack detected; aborting authentication"

            let githubClientID = "416fdf5ed5fb66f16bd3"
            let githubClientSecret = "298f94844d493cc1deccf97ba54a268d1b5690a8"

            let url = "http://34.208.168.142:3000"
            let params = [ ("client_id"    , BSChar8.pack githubClientID)
                         , ("client_secret", BSChar8.pack githubClientSecret)
                         , ("code"         , BSChar8.pack accessTokenRequestCode)
                         ]

            let githubUrl = "https://github.com/login/oauth/access_token"
            resultBody <- HTTP.responseBody
                <$> sendHTTPRequest githubUrl params id methodPost
            let decodedResultBody = Network.CGI.formDecode . BSChar8.unpack . L.toStrict $ resultBody

            accessToken <- hoistEither . U.note "Fatal: couldn't extract access token" $
                snd <$> List.find ((==) "access_token" . fst) decodedResultBody
            user <- getUserForToken accessToken
            let userID = fromIntegral $ User.id user
            TokenStore.insert connectionPool $ Token.OAuth2Token userID accessToken
        case result of
            (Left msg) -> text $ T.pack msg
            (Right _) -> text "success"

    notFound $ text "there is no such route."
    where
        getUserForToken :: String -> EIO User.BigUser
        getUserForToken accessToken = do
            let addHeaders r = r { HTTP.requestHeaders = [("User-Agent", "hueue"), ("Authorization", BSChar8.pack ("token " ++ accessToken))]}
            userJSON <- HTTP.responseBody <$> sendHTTPRequest "https://api.github.com/user" [] addHeaders methodGet
            (hoistEither . A.eitherDecode $ userJSON) :: EIO User.BigUser

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
