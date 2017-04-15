{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module DataStore.TokenStore
( loadByUserID
, DataStore.TokenStore.insert
) where

import qualified Data.Default as Default
import qualified Data.Serialize as Serialize

import MonadImports

import Aliases

import Data.Maybe

import qualified DataStore.Token as Token

import qualified Utils as U

import qualified Data.Text as T

import Database.Persist
import Database.Persist.TH
import Database.Persist.Sqlite
import Control.Monad.Logger (runStderrLoggingT)

loadByUserID :: ConnectionPool -> Int -> EIO (Maybe Token.OAuth2Token)
loadByUserID connectionPool githubUserID = liftIO . runStderrLoggingT $ do
    let action = getBy $ Token.UniqueUserID githubUserID
    runSqlPool (fmap (\(Entity k t) -> t) <$> action) connectionPool

insert :: ConnectionPool -> Token.OAuth2Token -> EIO ()
insert connectionPool token = liftIO . runStderrLoggingT . void $
    runSqlPool (Database.Persist.Sqlite.insert token) connectionPool
