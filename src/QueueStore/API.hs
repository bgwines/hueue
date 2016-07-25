{-# LANGUAGE RankNTypes #-}

module QueueStore.API
( enqueue
, dequeue
) where

import qualified Data.Default as Default
import qualified Data.Serialize as Serialize

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Either

import QueueStore.Types.Aliases (EIO, Error)

import qualified QueueStore.Store as Store

import qualified QueueStore.Constants as Constants

import qualified QueueStore.Types.Job as Job
import qualified QueueStore.Types.JobQueue as JobQueue
import QueueStore.Types.JobQueue ((<:>))

import qualified GithubWebhook.Types.Repo as Repo

import qualified Utils as U

enqueue :: Job.Job -> EIO ()
enqueue job = do
    jobQueue <- liftIO $ Store.loadOrNewQueue (Job.repo job)
    U.printIO jobQueue
    Store.writeQueue (Job.repo job) (job <:> jobQueue)

dequeue :: Repo.Repo -> EIO Job.Job
dequeue repo = do
    jobQueue <- Store.loadQueue repo
    when (JobQueue.null jobQueue) $ do
        left "Can't dequeue from an empty queue"

    let job = JobQueue.head jobQueue
    Store.writeQueue (Job.repo job) (JobQueue.tail jobQueue)
    right job
