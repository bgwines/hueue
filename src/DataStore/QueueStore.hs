{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module DataStore.QueueStore
( enqueue
, dequeue
, dequeueAll
) where

import qualified Data.Default as Default
import qualified Data.Serialize as Serialize

import MonadImports

import Aliases

import qualified DataStore.Job as Job

import qualified GithubWebhook.Types.Repo as Repo

import qualified Utils as U

import Database.Persist
import Database.Persist.TH
import Database.Persist.Sqlite
import Control.Monad.Logger (runStderrLoggingT)

enqueue :: ConnectionPool -> Job.Job -> EIO ()
enqueue connectionPool job = liftIO . runStderrLoggingT $ do
    runSqlPool (insert job) connectionPool
    return ()

dequeue :: ConnectionPool -> Repo.Repo -> EIO Job.Job
dequeue connectionPool repo = liftIO . runStderrLoggingT $ do
    let repoID = fromIntegral $ Repo.id repo
    let action = selectList [Job.JobRepoID ==. repoID] []
    jobs <- runSqlPool action connectionPool
    when (null jobs) $ fail "No jobs to dequeue"
    let Entity key job = head jobs
    runSqlPool (delete key) connectionPool
    return job

dequeueAll :: ConnectionPool -> Int -> EIO [Job.Job]
dequeueAll connectionPool repoID = liftIO . runStderrLoggingT $ do
    let action = selectList [Job.JobRepoID ==. repoID] []
    jobs <- runSqlPool action connectionPool
    mapM_ (\(Entity key job) -> runSqlPool (delete key) connectionPool) jobs
    return $ map (\(Entity key job) -> job) jobs
