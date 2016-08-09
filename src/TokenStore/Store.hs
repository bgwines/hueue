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

import qualified DataStore

-- TODO: RepoID newtype
loadToken :: Integer -> EIO BS.ByteString
loadToken repoID = DataStore.load (BSChar8.pack . show $ repoID) Constants.tokenStoreDBPath

writeToken :: BS.ByteString -> Integer -> EIO ()
writeToken token repoID
    = DataStore.write (BSChar8.pack . show $ repoID) token Constants.tokenStoreDBPath
