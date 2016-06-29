{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module GithubWebhook.Types.Commit
( Commit(..)
) where

import qualified Data.Text as T
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A

import GHC.Generics

import qualified GithubWebhook.Types.SmallUser as SmallUser

import qualified Utils

data Commit = Commit
    { id :: T.Text
    , treeId :: T.Text
    , distinct :: Bool
    , message :: T.Text
    , timestamp :: T.Text
    , url :: T.Text
    , author :: SmallUser.SmallUser
    , committer :: SmallUser.SmallUser
    , added :: [T.Text]
    , removed :: [T.Text]
    , modified :: [T.Text] } deriving (Eq, Generic, Show)

$(A.deriveJSON
    A.defaultOptions
    {A.fieldLabelModifier = Utils.camelCaseToSnakeCase}
    ''Commit)