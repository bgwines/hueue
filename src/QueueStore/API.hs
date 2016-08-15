{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module QueueStore.API
( enqueue
, dequeue
) where

import qualified Data.Default as Default
import qualified Data.Serialize as Serialize

import MonadImports

import Aliases

import qualified Job

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
    let action = (selectList [Job.JobRepoID ==. repoID] [])
    jobs <- runSqlPool action connectionPool
    let Entity key job = head jobs --- TODO: null check
    runSqlPool (delete key) connectionPool
    return job
