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

import Control.Monad.IO.Class
import Control.Monad.Trans.Either

import QueueStore.Types.Aliases (EIO, Error)

import qualified Utils as U

import qualified QueueStore.Types.Job as Job
import qualified QueueStore.Types.JobQueue as JobQueue
import qualified QueueStore.Constants as Constants

import qualified GithubWebhook.Types.Repo as Repo

repoID :: Repo.Repo -> BS.ByteString
repoID = BSChar8.pack . show . Repo.id

createIfMissing :: DB.Options
createIfMissing = DB.defaultOptions{ DB.createIfMissing = True }

loadOrNewQueue :: Repo.Repo -> IO JobQueue.JobQueue
loadOrNewQueue repo
    = either (const JobQueue.newEmptyJobQueue) id <$> runEitherT (loadQueue repo)

loadQueue :: Repo.Repo -> EIO JobQueue.JobQueue
loadQueue repo = do
    maybeQueue <- liftIO $ do
        db <- DB.open Constants.queueStoreDBPath createIfMissing
        maybeQueue <- DB.get db Default.def (repoID repo)
        DBInternal.unsafeClose db
        return maybeQueue
    hoistEither $ U.note errorMessage maybeQueue >>= Serialize.decode
    where
        errorMessage :: String
        errorMessage = "Could not fetch queue for repo with repoID " ++ show (repoID repo)

loadQueueDEBUG :: Int -> EIO JobQueue.JobQueue
loadQueueDEBUG repositoryID = do
    maybeQueue <- liftIO $ do
        db <- DB.open Constants.queueStoreDBPath createIfMissing
        maybeQueue <- DB.get db Default.def (BSChar8.pack . show $ repositoryID)
        DBInternal.unsafeClose db
        return maybeQueue
    hoistEither $ U.note errorMessage maybeQueue >>= Serialize.decode
    where
        errorMessage :: String
        errorMessage = "Could not fetch queue for repo with repoID " ++ show repositoryID

writeQueue :: Repo.Repo -> JobQueue.JobQueue -> EIO ()
writeQueue repo jobQueue = do
    liftIO $ do
        db <- DB.open Constants.queueStoreDBPath createIfMissing
        DB.put db Default.def (repoID repo) (Serialize.encode jobQueue)
        DBInternal.unsafeClose db

clearQueueDEBUG :: Int -> EIO ()
clearQueueDEBUG repositoryID = do
    liftIO $ do
        db <- DB.open Constants.queueStoreDBPath createIfMissing
        DB.put db Default.def (BSChar8.pack . show $ repositoryID) (Serialize.encode JobQueue.newEmptyJobQueue)
        DBInternal.unsafeClose db
