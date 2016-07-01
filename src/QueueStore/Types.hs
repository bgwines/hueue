{-# LANGUAGE DeriveGeneric #-}

module QueueStore.Types
( Error
, EIO
, JobQueue(..)
, Job(..)
) where

import qualified Data.Text as T
import qualified Data.Serialize as S

-- Serialize instance for Data.Text
import Data.Serialize.Text

import qualified GithubWebhook.Types.Repo as Repo

import GHC.Generics

import Control.Monad.Trans.Either

-- | Internal error type
type Error = String

-- | Shorthand
type EIO a = EitherT Error IO a

-- | Data type for the queues of tasks
data JobQueue = JobQueue { queue :: [Job] } deriving (Eq, Show, Generic)

-- For DB storage
instance S.Serialize JobQueue

-- | Data type for tasks
data Job = SafeMergeJob
    { repo :: Repo.Repo
    , srcBranch :: T.Text
    , dstBranch :: T.Text
    }
    deriving (Eq, Show, Generic)

-- For DB storage
instance S.Serialize Job
