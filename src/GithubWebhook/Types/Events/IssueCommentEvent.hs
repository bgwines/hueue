{-# LANGUAGE DeriveGeneric #-}

module GithubWebhook.Types.Events.IssueCommentEvent
( IssueCommentEvent(..)
) where

import qualified Data.Text as T
import qualified Data.Aeson as A

import qualified GithubWebhook.Types.SmallUser as SmallUser
import qualified GithubWebhook.Types.Repo as Repo
import qualified GithubWebhook.Types.Commit as Commit
import qualified GithubWebhook.Types.BigUser as BigUser

import GHC.Generics

data IssueCommentEvent = IssueCommentEvent
    { ref :: T.Text
    , before :: T.Text
    , after :: T.Text
    , created :: Bool
    , deleted :: Bool
    , forced :: Bool
    , base_ref :: Maybe T.Text
    , compare :: T.Text
    , commits :: [Commit.Commit]
    , head_commit :: Commit.Commit
    , repository :: Repo.Repo
    , pusher :: SmallUser.SmallUser
    , sender :: BigUser.BigUser } deriving (Eq, Generic, Show)

instance A.ToJSON IssueCommentEvent
instance A.FromJSON IssueCommentEvent
