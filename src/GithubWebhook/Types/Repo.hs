{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module GithubWebhook.Types.Repo
( Repo(..)
) where

import qualified Data.Text as T
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A

import qualified GithubWebhook.Types.BigUser as BigUser
import qualified GithubWebhook.Types.SmallUser as SmallUser

import qualified Utils

import GHC.Generics

data Repo = Repo
    { id :: Integer
    , name :: T.Text
    , fullName :: T.Text
    , owner :: BigUser.BigUser
    , private :: Bool
    , htmlUrl :: T.Text
    , description :: T.Text
    , fork :: Bool
    , url :: T.Text
    , forksUrl :: T.Text
    , keysUrl :: T.Text
    , collaboratorsUrl :: T.Text
    , teamsUrl :: T.Text
    , hooksUrl :: T.Text
    , issueEventsUrl :: T.Text
    , eventsUrl :: T.Text
    , assigneesUrl :: T.Text
    , branchesUrl :: T.Text
    , tagsUrl :: T.Text
    , blobsUrl :: T.Text
    , gitTagsUrl :: T.Text
    , gitRefsUrl :: T.Text
    , treesUrl :: T.Text
    , statusesUrl :: T.Text
    , languagesUrl :: T.Text
    , stargazersUrl :: T.Text
    , contributorsUrl :: T.Text
    , subscribersUrl :: T.Text
    , subscriptionUrl :: T.Text
    , commitsUrl :: T.Text
    , gitCommitsUrl :: T.Text
    , commentsUrl :: T.Text
    , issueCommentUrl :: T.Text
    , contentsUrl :: T.Text
    , compareUrl :: T.Text
    , mergesUrl :: T.Text
    , archiveUrl :: T.Text
    , downloadsUrl :: T.Text
    , issuesUrl :: T.Text
    , pullsUrl :: T.Text
    , milestonesUrl :: T.Text
    , notificationsUrl :: T.Text
    , labelsUrl :: T.Text
    , releasesUrl :: T.Text
    , deploymentsUrl :: T.Text
    , createdAt :: T.Text
    , updatedAt :: T.Text
    , pushedAt :: T.Text
    , gitUrl :: T.Text
    , sshUrl :: T.Text
    , cloneUrl :: T.Text
    , svnUrl :: T.Text
    , homepage :: Maybe T.Text
    , size :: Integer
    , stargazersCount :: Integer
    , watchersCount :: Integer
    , language :: Maybe T.Text
    , hasIssues :: Bool
    , hasDownloads :: Bool
    , hasWiki :: Bool
    , hasPages :: Bool
    , forksCount :: Integer
    , mirrorUrl :: Maybe T.Text
    , openIssuesCount :: Integer
    , forks :: Integer
    , openIssues :: Integer
    , watchers :: Integer
    , defaultBranch :: T.Text
    , stargazers :: Maybe Integer
    , masterBranch :: Maybe T.Text } deriving (Eq, Generic, Show)

$(A.deriveJSON
    A.defaultOptions
    {A.fieldLabelModifier = Utils.camelCaseToSnakeCase}
    ''Repo)
