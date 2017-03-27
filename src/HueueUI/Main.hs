{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Yesod

import Database.Persist
import Database.Persist.TH
import Database.Persist.Sqlite

import Database.Persist.Sqlite
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger (runStderrLoggingT)

import Network.HTTP.Client.Conduit (Manager, newManager)

import qualified Token
import qualified Job
import HueueUI.Types

main :: IO ()
main = runStderrLoggingT $ do
    let githubClientID = "416fdf5ed5fb66f16bd3"
    let githubClientSecret = "298f94844d493cc1deccf97ba54a268d1b5690a8"
    let githubKeys = OAuthKeys githubClientID githubClientSecret

    -- TODO: get these
    let googleClientID = ""
    let googleClientSecret = ""
    let googleKeys = OAuthKeys googleClientID googleClientSecret

    connectionPool <- createSqlitePool "commonPool" 10
    runSqlPool (runMigration Job.migrateAll) connectionPool
    runSqlPool (runMigration Token.migrateAll) connectionPool

    httpManager <- newManager
    liftIO $ warp 3000 (HueueUI connectionPool httpManager githubKeys googleKeys)
