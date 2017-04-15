{-# LANGUAGE Rank2Types #-}

module DataStore.TokenStore
( loadByUserID
, insert
) where

import Aliases
import MonadImports
import qualified Database.Persist.Sqlite as P
import qualified DataStore.Token as Token
import qualified Utils as Utils
import qualified Executor

loadByUserID :: P.ConnectionPool -> Int -> EIO (Maybe Token.OAuth2Token)
loadByUserID connectionPool githubUserID = do
    let action = fmap Utils.getEntityValue <$> P.getBy (Token.UniqueUserID githubUserID)
    Executor.execDB connectionPool action

insert :: P.ConnectionPool -> Token.OAuth2Token -> EIO ()
insert connectionPool token = void $ Executor.execDB connectionPool (P.insert token)
