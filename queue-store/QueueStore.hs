module QueueStore
( insert
, load
) where

-- TODO: access over network; don't make a library

import qualified Data.Default as Default
import qualified Data.Serialize as Serialize

import qualified Database.LevelDB.Base as DB
import qualified Database.LevelDB.Internal as DBInternal

import Control.Monad.IO.Class
import Control.Monad.Trans.Either

import qualified Utils as U

import QueueStoreTypes
import qualified QueueStoreConstants as Constants

insert :: RepoID -> JobQueue -> EIO ()
insert repoID queue = do
    liftIO $ do
        db <- DB.open Constants.queueStoreDBPath Default.def
        DB.put db Default.def repoID (Serialize.encode queue)
        DBInternal.unsafeClose db

load :: RepoID -> EIO JobQueue
load repoID = do
    maybeQueue <- liftIO $ do
        db <- DB.open Constants.queueStoreDBPath Default.def
        maybeQueue <- DB.get db Default.def repoID
        DBInternal.unsafeClose db
        return maybeQueue
    hoistEither $ U.note errorMessage maybeQueue >>= Serialize.decode
    where
        errorMessage :: String
        errorMessage = "Could not fetch queue for repo with repoID " ++ show repoID
