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

import qualified QueueStore.API
import qualified QueueStore.Types.Job

requestToJob :: RequestParser.ParsedRequest -> Repo.Repo -> QueueStore.Types.Job.Job
requestToJob (RequestParser.ParsedRequest _task srcBranch dstBranch) repo
    = QueueStore.Types.Job.SafeMergeJob repo srcBranch dstBranch

handleIssueComment :: (MonadIO m) => ICEvent.IssueCommentEvent -> EitherT Error m ()
handleIssueComment event = do
    liftIO . putStrLn $ "Handling an issue comment!"
    request <- hoistEither . RequestParser.parse . Comment.body . ICEvent.comment $ event
    liftIO . print $ request

    QueueStore.API.enqueue $ requestToJob request (ICEvent.repository event)
