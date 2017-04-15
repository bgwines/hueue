{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Aliases
import Data.Aeson (toJSON)
import MonadImports
import qualified Data.Text.Lazy as TL
import qualified Database.Persist.Sqlite as P
import qualified DataStore.Job as Job
import qualified DataStore.JobStore as JobStore
import qualified DataStore.Repo as Repo
import qualified DataStore.RepoStore as RepoStore
import qualified GithubOAuth
import qualified GithubWebhook.Types.BigUser as User
import qualified Executor
import qualified Web.Scotty as Scotty
import qualified Utils

main :: IO ()
main = Executor.serve $ serverAction 3000

serverAction :: Int -> P.ConnectionPool -> IO ()
serverAction port connectionPool = Scotty.scotty port $ do
    Scotty.get "/" $ liftIO (TL.pack <$> readFile "src/HueueUI/index.html") >>= Scotty.html

    Scotty.get "/get_jobs" $ do
        eitherJobs <- runEitherT $ do
            repos <- currUserGithubUserID >>= RepoStore.loadByGithubUserID connectionPool
            concat <$> mapM (JobStore.loadByRepo connectionPool) repos
        Scotty.json . map toJSON . Utils.getRightOrElse [] $ eitherJobs

    Scotty.get "/oauthRedirect" $ do
        accessTokenRequestCode :: String <- Scotty.param "code"
        state :: String <- Scotty.param "state"
        result <- runEitherT $ GithubOAuth.handleOAuthRedirect connectionPool port accessTokenRequestCode state
        case result of
            (Left msg) -> Scotty.text $ TL.pack msg
            (Right _) -> Scotty.text "success"

    Scotty.notFound $ Scotty.text "there is no such route."

-- constanty things

currUserGithubUserID :: (MonadIO m) => m Int
currUserGithubUserID = return 2442246
