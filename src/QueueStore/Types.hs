{-# LANGUAGE DeriveGeneric #-}

module QueueStore.Types
( Error
, EIO
, JobQueue(..)
, RepoID
) where

import qualified Data.Text as T
import qualified Data.Serialize as S
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI

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
    { repoID :: RepoID
    , srcBranch :: BS.ByteString
    , dstBranch :: BS.ByteString
    }
    deriving (Eq, Show, Generic)

-- For DB storage
instance S.Serialize Job

-- | Key in the
type RepoID = BSI.ByteString
