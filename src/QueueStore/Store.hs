{-# LANGUAGE RankNTypes #-}

module QueueStore.Store
( loadQueue
, loadOrNewQueue
, writeQueue
, clearQueueDEBUG
, loadQueueDEBUG
) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSChar8

import qualified Data.Default as Default
import qualified Data.Serialize as Serialize

import qualified Database.LevelDB.Base as DB
import qualified Database.LevelDB.Internal as DBInternal

import MonadImports

import Aliases

import qualified Utils as U

import qualified QueueStore.Types.Job as Job
import qualified QueueStore.Types.JobQueue as JobQueue
import qualified QueueStore.Constants as Constants

import qualified GithubWebhook.Types.Repo as Repo

import qualified DataStore

repoID :: Repo.Repo -> BS.ByteString
repoID = BSChar8.pack . show . Repo.id

loadOrNewQueue :: Repo.Repo -> IO JobQueue.JobQueue
loadOrNewQueue repo
    = either (const JobQueue.newEmptyJobQueue) id <$> runEitherT (loadQueue repo)

loadQueue :: Repo.Repo -> EIO JobQueue.JobQueue
loadQueue repo = do
    DataStore.load (BSChar8.pack . show $ repoID repo) Constants.queueStoreDBPath >>= (hoistEither . Serialize.decode)

loadQueueDEBUG :: Int -> EIO JobQueue.JobQueue
loadQueueDEBUG repositoryID = do
    DataStore.load (BSChar8.pack . show $ repositoryID) Constants.queueStoreDBPath >>= (hoistEither . Serialize.decode)

writeQueue :: Repo.Repo -> JobQueue.JobQueue -> EIO ()
writeQueue repo jobQueue = DataStore.write (BSChar8.pack . show $ repoID repo) (Serialize.encode jobQueue) Constants.queueStoreDBPath

clearQueueDEBUG :: Int -> EIO ()
clearQueueDEBUG repositoryID = DataStore.write (BSChar8.pack . show $ repositoryID) (Serialize.encode JobQueue.newEmptyJobQueue) Constants.queueStoreDBPath
