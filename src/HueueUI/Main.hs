{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Database.Persist hiding (get)
import Database.Persist.TH
import Database.Persist.Sqlite hiding (get)

import Control.Monad.Trans.Reader

import Control.Monad.IO.Class
import Control.Monad.Logger (runStderrLoggingT)

import qualified Data.Text.Lazy as T

import qualified Token
import qualified Job

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
    runSqlPool (runMigration Job.migrateAll >> runMigration Token.migrateAll) connectionPool
    liftIO $ serve 3000 connectionPool

serve :: Int -> ConnectionPool -> IO ()
serve port connectionPool = scotty port $ do
    get "/" $ do
        liftIO (T.pack <$> readFile "src/HueueUI/index.html") >>= html

    get "/get_jobs" $ do
        let action = selectList [Job.JobRepoID ==. 61999075] []
        jobs <- runSqlPool action connectionPool
        json $ map show jobs

    get "/:word" $ do
        beam <- param "word"
        html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]

    notFound $ do
         text "there is no such route."
