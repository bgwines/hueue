module GithubWebhook.Producer
( handleIssueComment
, handlePush
) where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Internal as BSLI

import Data.Either
import Data.Monoid
import Control.Monad
import Control.Applicative
import Control.Conditional (whenM, unlessM)
import Control.Monad.IO.Class
import Control.Monad.Trans.Either

import GithubWebhook.Types.Error (Error)

import qualified GithubWebhook.RequestParser as RequestParser

import qualified GithubWebhook.Types.Repo as Repo
import qualified GithubWebhook.Types.Comment as Comment
import qualified GithubWebhook.Types.Events.PushEvent as PEvent
import qualified GithubWebhook.Types.Events.IssueCommentEvent as ICEvent

import qualified Utils as U

import qualified QueueStore.API
import qualified QueueStore.Types

requestToJob :: RequestParser.ParsedRequest -> Repo.Repo -> QueueStore.Types.Job
requestToJob (RequestParser.ParsedRequest _task srcBranch dstBranch) repo
    = QueueStore.Types.SafeMergeJob repo srcBranch dstBranch

handleIssueComment :: ICEvent.IssueCommentEvent -> EitherT Error IO ()
handleIssueComment event = do
    U.putStrLnIO $ "Handling an issue comment!"
    request <- hoistEither . RequestParser.parse . Comment.body . ICEvent.comment $ event
    U.printIO request

    -- TODO: over network
    QueueStore.API.enqueue $ requestToJob request (ICEvent.repository event)

handlePush :: PEvent.PushEvent -> EitherT Error IO ()
handlePush event = do
    U.putStrLnIO $ "Handling a push!"
    -- Do nothing
    return ()
