{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module DataStore.JobStore
( loadByRepo
, DataStore.JobStore.insert
) where

import Aliases
import Control.Monad.Logger (runStderrLoggingT)
import MonadImports
import qualified Database.Persist.Sqlite as P
import qualified DataStore.Job as Job
import qualified DataStore.Repo as Repo
import qualified Executor
import qualified Utils

loadByRepo :: P.ConnectionPool -> Repo.Repo -> EIO [Job.Job]
loadByRepo connectionPool (Repo.Repo repoID _) = liftIO . runStderrLoggingT $ do
    let action = P.selectList [Job.JobRepoID P.==. repoID] []
    map Utils.getEntityValue <$> P.runSqlPool action connectionPool

insert :: MonadIO m => P.ConnectionPool -> Job.Job -> m ()
insert connectionPool job = Executor.execDB connectionPool (void $ P.insert job)
