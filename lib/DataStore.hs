{-# LANGUAGE RankNTypes #-}

module DataStore
( load
, write
) where

import qualified Data.ByteString as BS

import qualified Data.Default as Default
import qualified Data.Serialize as Serialize

import qualified Database.LevelDB.Base as DB
import qualified Database.LevelDB.Internal as DBInternal

import MonadImports

import Aliases

import qualified Utils as U

load :: BS.ByteString -> FilePath -> EIO BS.ByteString
load key dbPath =
    (hoistEither . U.note errorMessage) =<< liftIO (do
        db <- DB.open dbPath (DB.defaultOptions{ DB.createIfMissing = True })
        maybeValue <- DB.get db Default.def key
        DBInternal.unsafeClose db
        return maybeValue)
    where
        errorMessage :: String
        errorMessage = "Could not fetch value for key " ++ show key

write :: BS.ByteString -> BS.ByteString -> FilePath -> EIO ()
write value key dbPath = liftIO $ do
    db <- DB.open dbPath (DB.defaultOptions{ DB.createIfMissing = True })
    DB.put db Default.def key value
    DBInternal.unsafeClose db
