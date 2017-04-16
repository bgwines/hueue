{-# LANGUAGE Rank2Types #-}

module DataStore.RepoStore
( loadByGithubUserID
, loadByRepoID
, DataStore.RepoStore.insert
, DataStore.RepoStore.convert
) where

import Aliases
import MonadImports
import qualified Database.Persist.Sqlite as P
import qualified DataStore.Job as Job
import qualified DataStore.Repo as Repo
import qualified GithubWebhook.Types.Repo as WebhookRepo
import qualified Executor
import qualified Utils

loadByGithubUserID :: P.ConnectionPool -> Int -> EIO [Repo.Repo]
loadByGithubUserID connectionPool githubUserID = do
    let filters = [Repo.RepoMergerGithubUserID P.==. githubUserID]
    let action = map Utils.getEntityValue <$> P.selectList filters []
    Executor.execDB connectionPool action

loadByRepoID :: P.ConnectionPool -> Int -> EIO [Repo.Repo]
loadByRepoID connectionPool repoID = do
    let action = map Utils.getEntityValue <$> P.selectList [Repo.RepoRepoID P.==. repoID] []
    Executor.execDB connectionPool action

-- dupes?
insert :: P.ConnectionPool -> Repo.Repo -> EIO ()
insert connectionPool repo@(Repo.Repo repoID _ githubUserID) = do
    let sameUserID = Repo.RepoMergerGithubUserID P.==. githubUserID
    let sameRepoID = Repo.RepoRepoID P.==. repoID
    let filters = [P.FilterAnd [sameUserID, sameRepoID]]
    let action = length <$> P.selectList filters []
    numRepos <- Executor.execDB connectionPool action
    unless (numRepos > 0) $ do
        void $ Executor.execDB connectionPool (P.insert repo)

convert :: WebhookRepo.Repo -> Int -> Repo.Repo
convert webhookRepo githubUserID = Repo.Repo repoID (WebhookRepo.name webhookRepo) githubUserID
    where
        repoID = fromInteger . WebhookRepo.id $ webhookRepo
