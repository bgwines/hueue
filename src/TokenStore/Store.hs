{-# LANGUAGE RankNTypes #-}

module TokenStore.Store
( loadToken
, writeToken
) where

import qualified Data.Convertible as C

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSChar8

import qualified Data.Default as Default
import qualified Data.Serialize as Serialize

import qualified Database.LevelDB.Base as DB
import qualified Database.LevelDB.Internal as DBInternal

import MonadImports

import Aliases

import qualified Utils as U

import qualified TokenStore.Constants as Constants

import qualified GithubWebhook.Types.Repo as Repo

createIfMissing :: DB.Options
createIfMissing = DB.defaultOptions{ DB.createIfMissing = True }

-- TODO: RepoID newtype
-- TODO: share with QueueStore.Store
loadToken :: Integer -> EIO BS.ByteString
loadToken repoID = do
    maybeToken <- liftIO $ do
        db <- DB.open Constants.tokenStoreDBPath createIfMissing
        maybeToken <- DB.get db Default.def (BSChar8.pack . show $ repoID)
        DBInternal.unsafeClose db
        return maybeToken
    hoistEither $ U.note errorMessage maybeToken
    where
        errorMessage :: String
        errorMessage = "Could not fetch token for repo with repoID " ++ show repoID

writeToken :: BS.ByteString -> Integer -> EIO ()
writeToken token repoID = liftIO $ do
    db <- DB.open Constants.tokenStoreDBPath createIfMissing
    DB.put db Default.def (BSChar8.pack . show $ repoID) token
    DBInternal.unsafeClose db
