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
    connectionPool <- createSqlitePool "commonPool" 10
    runSqlPool (runMigration Job.migrateAll) connectionPool
    runSqlPool (runMigration Token.migrateAll) connectionPool
    liftIO $ warp 3000 (HueueUI connectionPool)
