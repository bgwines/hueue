{-# LANGUAGE OverloadedStrings #-}

module Executor
( serve
, execDB
) where

import Control.Monad.Logger (runStderrLoggingT, LoggingT)
import MonadImports
import qualified Database.Persist.Sqlite as P
import qualified DataStore.Job as Job
import qualified DataStore.Repo as Repo
import qualified DataStore.Token as Token

serve :: (P.ConnectionPool -> IO ()) -> IO ()
serve serverAction = runStderrLoggingT $ do
    connectionPool <- P.createSqlitePool "commonPool" 10
    P.runSqlPool
        (  P.runMigration Job.migrateAll
        >> P.runMigration Repo.migrateAll
        >> P.runMigration Token.migrateAll ) connectionPool
    liftIO $ serverAction connectionPool

execDB :: MonadIO m => P.ConnectionPool -> P.SqlPersistT (LoggingT IO) a -> m a
execDB connectionPool action = liftIO . runStderrLoggingT $
    P.runSqlPool action connectionPool
