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

import qualified GithubWebhook.Types.Repo as WebhookRepo
import qualified GithubWebhook.Types.Comment as Comment
import qualified GithubWebhook.Types.BigUser as BigUser
import qualified GithubWebhook.Types.Events.PushEvent as PEvent
import qualified GithubWebhook.Types.Events.IssueCommentEvent as ICEvent

import qualified Utils as U

import Aliases

import qualified DataStore.Job as Job
import qualified DataStore.RepoStore as RepoStore
import qualified DataStore.QueueStore as QueueStore

import Database.Persist
import Database.Persist.TH
import Database.Persist.Sqlite

requestToJob :: RequestParser.ParsedRequest -> WebhookRepo.Repo -> Job.Job
requestToJob (RequestParser.ParsedRequest _task srcBranch dstBranch) repo
    = Job.Job (fromIntegral $ WebhookRepo.id repo) srcBranch dstBranch

handleIssueComment :: ConnectionPool -> ICEvent.IssueCommentEvent -> EIO ()
handleIssueComment connectionPool event = do
    U.putStrLnIO "Handling an issue comment!"
    request <- hoistEither . RequestParser.parse . Comment.body . ICEvent.comment $ event
    U.printIO request
    let userID = fromIntegral . BigUser.id . Comment.user . ICEvent.comment $ event
    let repo = ICEvent.repository event
    RepoStore.insert connectionPool $ RepoStore.convert repo userID
    QueueStore.enqueue connectionPool $ requestToJob request repo
