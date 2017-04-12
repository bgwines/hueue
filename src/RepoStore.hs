{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module RepoStore
( loadByGithubUserID
, RepoStore.insert
, RepoStore.convert
) where

import qualified Data.Default as Default
import qualified Data.Serialize as Serialize

import MonadImports

import Aliases

import Data.Maybe

import qualified Repo
import qualified GithubWebhook.Types.BigUser as BigUser
import qualified GithubWebhook.Types.Repo as WebhookRepo

import qualified Utils as U

import qualified Data.Text as T

import Database.Persist
import Database.Persist.TH
import Database.Persist.Sqlite
import Control.Monad.Logger (runStderrLoggingT)

loadByGithubUserID :: ConnectionPool -> Int -> EIO [Repo.Repo]
loadByGithubUserID connectionPool githubUserID = liftIO . runStderrLoggingT $ do
    let action = selectList [Repo.RepoMergerGithubUserID ==. githubUserID] []
    map (\(Entity key repo) -> repo) <$> runSqlPool action connectionPool

-- dupes?
insert :: ConnectionPool -> Repo.Repo -> EIO ()
insert connectionPool repo = liftIO . runStderrLoggingT $
    void $ runSqlPool (Database.Persist.Sqlite.insert repo) connectionPool

convert :: WebhookRepo.Repo -> Int -> Repo.Repo
convert webhookRepo = Repo.Repo (fromInteger . WebhookRepo.id $ webhookRepo)
