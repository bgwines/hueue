{-# LANGUAGE Rank2Types #-}

module DataStore.JobStore
( loadByRepo
, insert
) where

import Aliases
import MonadImports
import qualified Database.Persist.Sqlite as P
import qualified DataStore.Job as Job
import qualified DataStore.Repo as Repo
import qualified Executor
import qualified Utils

loadByRepo :: P.ConnectionPool -> Repo.Repo -> EIO [Job.Job]
loadByRepo connectionPool (Repo.Repo repoID _) = do
    let action = (map Utils.getEntityValue) <$> P.selectList [Job.JobRepoID P.==. repoID] []
    Executor.execDB connectionPool action

insert :: MonadIO m => P.ConnectionPool -> Job.Job -> m ()
insert connectionPool job = Executor.execDB connectionPool (void $ P.insert job)
