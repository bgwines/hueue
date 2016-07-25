{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Consumer.Merger (merge) where

import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy.Internal as BSLI

import GHC.Generics

import MonadImports

import qualified QueueStore.Types.Job as Job
import qualified GithubWebhook.Types.Repo as Repo

import Aliases

import qualified Git.API as Git

import qualified Utils as U

import qualified System.Directory as D

merge :: Job.Job -> EIO ()
merge (Job.SafeMergeJob repo srcBranch dstBranch) = liftIO $ do
    originalDirectory <- D.getCurrentDirectory

    putStrLn "Cloning repository..."
    Git.clone (Repo.cloneUrl repo)

    putStrLn "Changing directories..."
    D.setCurrentDirectory $ T.unpack $ Repo.name repo

    putStrLn "Checking out branch..."
    Git.checkout srcBranch

    putStrLn "Rebasing..."
    Git.rebase dstBranch

    putStrLn "Forcepushing..."
    Git.forcePush

    -- TODO: wait

    putStrLn "Checking out destination branch..."
    Git.checkout dstBranch

    putStrLn "Merging..."
    Git.merge srcBranch

    putStrLn "Pushing..."
    Git.push

    putStrLn "Changing directories back..."
    D.setCurrentDirectory originalDirectory
