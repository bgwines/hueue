{-# LANGUAGE DeriveGeneric #-}

module GithubWebhook.Types.Events.IssueCommentEvent
( IssueCommentEvent(..)
) where

import qualified Data.Text as T
import qualified Data.Aeson as A

import qualified GithubWebhook.Types.User as U
import qualified GithubWebhook.Types.Repo as R
import qualified GithubWebhook.Types.Commit as C
import qualified GithubWebhook.Types.Sender as S

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
    , commits :: [C.Commit]
    , head_commit :: C.Commit
    , repository :: R.Repo
    , pusher :: U.User
    , sender :: S.Sender } deriving (Eq, Generic, Show)

instance A.ToJSON IssueCommentEvent
instance A.FromJSON IssueCommentEvent
