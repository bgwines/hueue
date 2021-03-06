{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Aliases
import Data.Aeson (toJSON, ToJSON, object, (.=), Value)
import MonadImports
import qualified Data.Text.Lazy as TL
import qualified Database.Persist.Sqlite as P
import qualified DataStore.Job as Job
import qualified DataStore.JobState as JobState
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
serverAction port conn = Scotty.scotty port $ do
    Scotty.get "/" $ liftIO (TL.pack <$> readFile "src/HueueUI/index.html") >>= Scotty.html

    Scotty.get "/getJobs" $ do
        eitherJobs <- runEitherT $ do
            repos <- currUserGithubUserID >>= RepoStore.loadByGithubUserID conn
            let repoToJobs r = map (displayJob r) <$> JobStore.loadEntitiesByRepo conn r
            concat <$> mapM repoToJobs repos
        jobs <- case eitherJobs of
            (Left message) -> Utils.printIO message >> return []
            (Right theJobs) -> return theJobs
        Scotty.json jobs

    Scotty.post "/killJob" $ do
        key :: Int <- Scotty.param "key"
        result <- runEitherT $ JobStore.updateState conn key JobState.Killed
        case result of
            (Left msg) -> Scotty.text $ TL.pack msg
            (Right ()) -> Scotty.text $ TL.pack $ "Successfully killed job " ++ show key

    Scotty.post "/suspendJob" $ do
        key :: Int <- Scotty.param "key"
        result <- runEitherT $ JobStore.updateState conn key JobState.Waiting
        case result of
            (Left msg) -> Scotty.text $ TL.pack msg
            (Right ()) -> Scotty.text $ TL.pack $ "Successfully suspended job " ++ show key

    Scotty.post "/resumeJob" $ do
        key :: Int <- Scotty.param "key"
        result <- runEitherT $ JobStore.updateState conn key JobState.Running
        case result of
            (Left msg) -> Scotty.text $ TL.pack msg
            (Right ()) -> Scotty.text $ TL.pack $ "Successfully resumed job " ++ show key

    Scotty.get "/oauthRedirect" $ do
        accessTokenRequestCode :: String <- Scotty.param "code"
        state :: String <- Scotty.param "state"
        result <- runEitherT $ GithubOAuth.handleOAuthRedirect conn port accessTokenRequestCode state
        case result of
            (Left msg) -> Scotty.text $ TL.pack msg
            (Right _) -> Scotty.text "success"

    Scotty.notFound $ Scotty.text "there is no such route."

-- constanty things

currUserGithubUserID :: (MonadIO m) => m Int
currUserGithubUserID = return 2442246

displayJob :: Repo.Repo -> P.Entity Job.Job -> Value
displayJob repo (P.Entity key job) = object
    [ "repoName" .= Repo.repoName repo
    , "srcBranch" .= Job.jobSrcBranch job
    , "dstBranch" .= Job.jobDstBranch job
    , "state" .= show (Job.jobState job)
    , "key" .= (P.unSqlBackendKey . Job.unJobKey $ key)
    ]
