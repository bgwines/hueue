{-# LANGUAGE DeriveGeneric #-}

module GithubWebhook.Types.Events.IssueCommentEvent
( IssueCommentEvent(..)
) where

import qualified Data.Text as T
import qualified Data.Aeson as A

import qualified GithubWebhook.Types.Repo as Repo
import qualified GithubWebhook.Types.Issue as Issue
import qualified GithubWebhook.Types.Commit as Commit
import qualified GithubWebhook.Types.Comment as Comment
import qualified GithubWebhook.Types.BigUser as BigUser
import qualified GithubWebhook.Types.SmallUser as SmallUser

import GHC.Generics

data IssueCommentEvent = IssueCommentEvent
    { action :: T.Text
    , issue :: Issue.Issue
    , comment :: Comment.Comment
    , repository :: Repo.Repo
    , sender :: BigUser.BigUser } deriving (Eq, Generic, Show)

instance A.ToJSON IssueCommentEvent
instance A.FromJSON IssueCommentEvent
