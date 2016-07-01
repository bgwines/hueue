{-# LANGUAGE DeriveGeneric #-}

module QueueStore.Types.Job
( Job(..)
) where

import GHC.Generics

import qualified Data.Text as T
import qualified Data.Serialize as S

import qualified GithubWebhook.Types.Repo as Repo

-- | Data type for tasks
data Job = SafeMergeJob
    { repo :: Repo.Repo
    , srcBranch :: T.Text
    , dstBranch :: T.Text
    }
    deriving (Eq, Show, Generic)

-- For DB storage
instance S.Serialize Job
