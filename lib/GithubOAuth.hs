{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GithubOAuth (handleOAuthRedirect) where

import Aliases
import MonadImports
import qualified Database.Persist.Sqlite as P
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSChar8
import qualified Data.ByteString.Lazy as BSL
import qualified Network.CGI (formDecode)
import qualified GithubWebhook.Types.BigUser as User
import qualified HTTPUtils
import qualified Utils as U
import qualified Data.List as List
import qualified Network.HTTP.Conduit as HTTP
import qualified DataStore.Token as Token
import qualified DataStore.TokenStore as TokenStore
import qualified Web.Scotty as Scotty

getUserForToken :: String -> EIO User.BigUser
getUserForToken accessToken = do
    let headers = [ ("User-Agent", "hueue")
                  , ("Authorization", BSChar8.pack ("token " ++ accessToken))]
    let addHeaders r = r {HTTP.requestHeaders = headers}
    let url = "https://api.github.com/user"
    userJSON <- HTTP.responseBody <$> HTTPUtils.sendHTTPRequest url [] addHeaders HTTPUtils.methodGet
    hoistEither . A.eitherDecode $ userJSON

handleOAuthRedirect :: P.ConnectionPool -> Int -> String -> String -> EIO ()
handleOAuthRedirect connectionPool port accessTokenRequestCode state = do
    unless (state == "142857") $
        left "Fatal: security attack detected; aborting authentication"

    let url = serverURL ++ ":" ++ show port
    let params = [ ("client_id"    , BSChar8.pack githubClientID)
                 , ("client_secret", BSChar8.pack githubClientSecret)
                 , ("code"         , BSChar8.pack accessTokenRequestCode)
                 ]

    let githubUrl = "https://github.com/login/oauth/access_token"
    resultBody <- HTTP.responseBody
        <$> HTTPUtils.sendHTTPRequest githubUrl params id HTTPUtils.methodPost
    let decodedResultBody = Network.CGI.formDecode . BSChar8.unpack . BSL.toStrict $ resultBody

    accessToken <- hoistEither . U.note "Fatal: couldn't extract access token" $
        snd <$> List.find ((==) "access_token" . fst) decodedResultBody
    user <- GithubOAuth.getUserForToken accessToken
    let userID = fromIntegral $ User.id user
    TokenStore.insert connectionPool $ Token.OAuth2Token userID accessToken

-- constanty things

currUserGithubUserID :: (MonadIO m) => m Int
currUserGithubUserID = return 2442246

serverURL :: String
serverURL = "http://34.208.168.142"

githubClientID :: String
githubClientID = "416fdf5ed5fb66f16bd3"

githubClientSecret :: String
githubClientSecret = "298f94844d493cc1deccf97ba54a268d1b5690a8"
