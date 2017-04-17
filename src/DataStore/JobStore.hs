{-# LANGUAGE Rank2Types #-}

module DataStore.JobStore
( loadByRepo
, loadEntitiesByRepo
, loadByRepos
, insert
, delete
, updateState
) where

import Aliases
import MonadImports
import qualified Database.Persist.Sqlite as P
import qualified DataStore.Job as Job
import qualified DataStore.JobState as JobState
import qualified DataStore.Repo as Repo
import qualified Executor
import qualified Utils

loadEntitiesByRepo :: P.ConnectionPool -> Repo.Repo -> EIO [P.Entity Job.Job]
loadEntitiesByRepo conn (Repo.Repo repoID _ _) = do
    let action = P.selectList [Job.JobRepoID P.==. repoID] []
    Executor.execDB conn action

loadByRepo :: P.ConnectionPool -> Repo.Repo -> EIO [Job.Job]
loadByRepo = fmap (map Utils.getEntityValue) ... loadEntitiesByRepo

loadByRepos :: P.ConnectionPool -> [Repo.Repo] -> EIO [Job.Job]
loadByRepos conn repos = concat <$> mapM (loadByRepo conn) repos

insert :: MonadIO m => P.ConnectionPool -> Job.Job -> m ()
insert conn job = Executor.execDB conn (void $ P.insert job)

delete :: P.ConnectionPool -> Int -> EIO ()
delete conn key = Executor.execDB conn (P.delete $ convertKey key)

updateState :: P.ConnectionPool -> Int -> JobState.JobState -> EIO ()
updateState conn key newState = Executor.execDB conn $
    P.update (convertKey key) [Job.JobState P.=. newState]

convertKey :: Int -> P.Key Job.Job
convertKey = Job.JobKey . P.SqlBackendKey . fromIntegral