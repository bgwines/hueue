{-# LANGUAGE DeriveGeneric #-}

module Repo
( Repo
) where

import qualified Data.Text as T
import qualified Data.Aeson as A

import qualified User as U

import GHC.Generics

data Repo = Repo
    { id :: Integer
    , name :: T.Text
    , full_name :: T.Text
    , owner :: U.User
    , private :: Bool
    , html_url :: T.Text
    , description :: T.Text
    , fork :: Bool
    , url :: T.Text
    , forks_url :: T.Text
    , keys_url :: T.Text
    , collaborators_url :: T.Text
    , teams_url :: T.Text
    , hooks_url :: T.Text
    , issue_events_url :: T.Text
    , events_url :: T.Text
    , assignees_url :: T.Text
    , branches_url :: T.Text
    , tags_url :: T.Text
    , blobs_url :: T.Text
    , git_tags_url :: T.Text
    , git_refs_url :: T.Text
    , trees_url :: T.Text
    , statuses_url :: T.Text
    , languages_url :: T.Text
    , stargazers_url :: T.Text
    , contributors_url :: T.Text
    , subscribers_url :: T.Text
    , subscription_url :: T.Text
    , commits_url :: T.Text
    , git_commits_url :: T.Text
    , comments_url :: T.Text
    , issue_comment_url :: T.Text
    , contents_url :: T.Text
    , compare_url :: T.Text
    , merges_url :: T.Text
    , archive_url :: T.Text
    , downloads_url :: T.Text
    , issues_url :: T.Text
    , pulls_url :: T.Text
    , milestones_url :: T.Text
    , notifications_url :: T.Text
    , labels_url :: T.Text
    , releases_url :: T.Text
    , deployments_url :: T.Text
    , created_at :: Integer
    , updated_at :: T.Text
    , pushed_at :: Integer
    , git_url :: T.Text
    , ssh_url :: T.Text
    , clone_url :: T.Text
    , svn_url :: T.Text
    , homepage :: Maybe T.Text
    , size :: Integer
    , stargazers_count :: Integer
    , watchers_count :: Integer
    , language :: Maybe T.Text
    , has_issues :: Bool
    , has_downloads :: Bool
    , has_wiki :: Bool
    , has_pages :: Bool
    , forks_count :: Integer
    , mirror_url :: Maybe T.Text
    , open_issues_count :: Integer
    , forks :: Integer
    , open_issues :: Integer
    , watchers :: Integer
    , default_branch :: T.Text
    , stargazers :: Integer
    , master_branch :: T.Text } deriving (Eq, Generic, Show)

instance A.ToJSON Repo
instance A.FromJSON Repo
