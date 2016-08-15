{-# LANGUAGE RankNTypes #-}

module GithubWebhook.Producer
( handleIssueComment
) where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Internal as BSLI

import MonadImports
import Control.Conditional (whenM, unlessM)

import GithubWebhook.Types.Error (Error)

import qualified GithubWebhook.RequestParser as RequestParser

import qualified GithubWebhook.Types.Repo as Repo
import qualified GithubWebhook.Types.Comment as Comment
import qualified GithubWebhook.Types.Events.PushEvent as PEvent
import qualified GithubWebhook.Types.Events.IssueCommentEvent as ICEvent

import qualified Utils as U

import qualified Job

import Aliases

import qualified QueueStore.API

import Database.Persist
import Database.Persist.TH
import Database.Persist.Sqlite

requestToJob :: RequestParser.ParsedRequest -> Repo.Repo -> Job.Job
requestToJob (RequestParser.ParsedRequest _task srcBranch dstBranch) repo
    = Job.Job (fromIntegral $ Repo.id repo) srcBranch dstBranch

handleIssueComment :: ConnectionPool -> ICEvent.IssueCommentEvent -> EIO ()
handleIssueComment connectionPool event = do
    U.putStrLnIO "Handling an issue comment!"
    request <- hoistEither . RequestParser.parse . Comment.body . ICEvent.comment $ event
    U.printIO request

    QueueStore.API.enqueue connectionPool $ requestToJob request (ICEvent.repository event)
