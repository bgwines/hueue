{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Database.Persist hiding (get)
import Database.Persist.TH
import Database.Persist.Sqlite hiding (get)

import Control.Monad.Trans.Reader

import Control.Monad.IO.Class
import Control.Monad.Logger (runStderrLoggingT)

import qualified Data.Text.Lazy as T

import qualified Job
import qualified Repo
import qualified Token
import qualified JobStore
import qualified RepoStore

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

    get "/:word" $ do
        beam <- param "word"
        html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]

    notFound $ text "there is no such route."
