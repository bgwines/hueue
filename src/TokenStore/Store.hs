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

import qualified GithubWebhook.Types.BigUser as User

import qualified DataStore

-- TODO: user ID newtype
loadToken :: User.BigUser -> EIO BS.ByteString
loadToken user = DataStore.load (BSChar8.pack . show . User.id $ user) Constants.tokenStoreDBPath

writeToken :: BS.ByteString -> User.BigUser -> EIO ()
writeToken token user
    = DataStore.write (BSChar8.pack . show . User.id $ user) token Constants.tokenStoreDBPath
