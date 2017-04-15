{-# LANGUAGE Rank2Types #-}

module DataStore.RepoStore
( loadByGithubUserID
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
    let action = do
        l <- P.selectList [Repo.RepoMergerGithubUserID P.==. githubUserID] []
        return $ map Utils.getEntityValue l
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
