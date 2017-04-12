{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module JobStore
( loadByRepoID
, JobStore.insert
) where

import qualified Data.Default as Default
import qualified Data.Serialize as Serialize

import MonadImports

import Aliases

import Data.Maybe

import qualified Job
import qualified GithubWebhook.Types.BigUser as BigUser
import qualified GithubWebhook.Types.Repo as WebhookRepo

import qualified Utils as U

import qualified Data.Text as T

import Database.Persist
import Database.Persist.TH
import Database.Persist.Sqlite
import Control.Monad.Logger (runStderrLoggingT)

loadByRepoID :: ConnectionPool -> Int -> EIO [Job.Job]
loadByRepoID connectionPool repoID = liftIO . runStderrLoggingT $ do
    let action = selectList [Job.JobRepoID ==. repoID] []
    map (\(Entity key job) -> job) <$> runSqlPool action connectionPool

-- dupes?
insert :: ConnectionPool -> Job.Job -> EIO ()
insert connectionPool job = liftIO . runStderrLoggingT $
    void $ runSqlPool (Database.Persist.Sqlite.insert job) connectionPool
