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
    deriving (Eq, Generic)

instance Show Job where
    show (SafeMergeJob repo srcBranch dstBranch)
        =  "Job "
        ++ T.unpack (Repo.fullName repo)
        ++ " "
        ++ T.unpack srcBranch
        ++ " "
        ++ T.unpack dstBranch
        ++ " "

-- For DB storage
instance S.Serialize Job
