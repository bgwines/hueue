{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module GithubWebhook.Types.Issue
( Issue(..)
) where

import qualified Data.Char as Ch
import qualified Data.Text as T
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A

import GHC.Generics

import qualified Utils

import qualified GithubWebhook.Types.User as User
import qualified GithubWebhook.Types.PullRequest as PullRequest

data Issue = Issue
    { url :: T.Text
    , repositoryUrl :: T.Text
    , labelsUrl :: T.Text
    , commentsUrl :: T.Text
    , eventsUrl :: T.Text
    , htmlUrl :: T.Text
    , id :: Integer
    , number :: Integer
    , title :: T.Text
    , user :: User.User
    , labels :: [T.Text] -- [???]
    , state :: T.Text
    , locked :: Bool
    , assignee :: Maybe User.User
    , assignees :: [User.User]
    , milestone :: Maybe T.Text -- [???]
    , comments :: Integer
    , createdAt :: T.Text
    , updatedAt :: T.Text
    , closedAt :: Maybe T.Text -- ???
    , pullRequest :: PullRequest.PullRequest
    , body :: T.Text
    } deriving (Eq, Generic, Show)

$(A.deriveJSON
    A.defaultOptions
    {A.fieldLabelModifier = Utils.camelCaseToSnakeCase}
    ''Issue)
