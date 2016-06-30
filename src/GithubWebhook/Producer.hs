module GithubWebhook.Producer
( handleIssueComment
, handlePush
) where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Internal as BSLI

import Data.Monoid
import Control.Monad
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Either

import GithubWebhook.Constants (Error)

import qualified GithubWebhook.Types.Comment as Comment
import qualified GithubWebhook.Types.Events.PushEvent as PEvent
import qualified GithubWebhook.Types.Events.IssueCommentEvent as ICEvent

import qualified Utils as U

handleIssueComment :: ICEvent.IssueCommentEvent -> EitherT Error IO ()
handleIssueComment event = do
    U.putStrLnIO $ "Handling an issue comment!"
    let body = Comment.body . ICEvent.comment $ event
    return ()

handlePush :: PEvent.PushEvent -> EitherT Error IO ()
handlePush event = do
    U.putStrLnIO $ "Handling a push!"
    return ()
