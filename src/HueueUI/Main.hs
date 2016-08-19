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

import qualified Token
import qualified Job
import HueueUI.Types

main :: IO ()
main = runStderrLoggingT $ do
    let clientID = "416fdf5ed5fb66f16bd3"
    let clientSecret = "298f94844d493cc1deccf97ba54a268d1b5690a8"
    let keys = OAuthKeys clientID clientSecret

    connectionPool <- createSqlitePool "commonPool" 10
    runSqlPool (runMigration Job.migrateAll) connectionPool
    runSqlPool (runMigration Token.migrateAll) connectionPool

    liftIO $ warp 3000 (HueueUI connectionPool keys)
