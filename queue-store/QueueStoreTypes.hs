{-# LANGUAGE DeriveGeneric #-}

module QueueStoreTypes
( Error
, EIO
, JobQueue(..)
, RepoID
) where

import qualified Data.Serialize as Serialize
import qualified Data.ByteString.Internal as BSI

import GHC.Generics

import Control.Monad.Trans.Either

type Error = String

-- | Shorthand.
type EIO a = EitherT Error IO a

data JobQueue = JobQueue { queue :: [Job] } deriving (Eq, Show, Generic)

instance Serialize.Serialize JobQueue

data Job = Job {} deriving (Eq, Show, Generic)

instance Serialize.Serialize Job

type RepoID = BSI.ByteString
