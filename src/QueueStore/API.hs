module QueueStore.API
( enqueue
, dequeue
) where

import qualified Data.Default as Default
import qualified Data.Serialize as Serialize

import qualified Database.LevelDB.Base as DB
import qualified Database.LevelDB.Internal as DBInternal

import Control.Monad.IO.Class
import Control.Monad.Trans.Either

import qualified Utils as U

import QueueStore.Types
import qualified QueueStore.Constants as Constants

import qualified GithubWebhook.Types.Repo as Repo

enqueue :: Job -> EIO ()
enqueue job = do
    right ()
    --liftIO $ do
    --    db <- DB.open Constants.queueStoreDBPath Default.def
    --    DB.put db Default.def repoID (Serialize.encode queue)
    --    DBInternal.unsafeClose db

dequeue :: Repo.Repo -> EIO Job
dequeue repo = do
    left "Not yet implemented"
    --maybeQueue <- liftIO $ do
    --    db <- DB.open Constants.queueStoreDBPath Default.def
    --    maybeQueue <- DB.get db Default.def repoID
    --    DBInternal.unsafeClose db
    --    return maybeQueue
    --hoistEither $ U.note errorMessage maybeQueue >>= Serialize.decode
    --where
    --    errorMessage :: String
    --    errorMessage = "Could not fetch queue for repo with repoID " ++ show repoID
