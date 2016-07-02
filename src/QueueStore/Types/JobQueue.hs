{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

module QueueStore.Types.JobQueue
( JobQueue(..)
, (<:>)
, head
, tail
, null
, newEmptyJobQueue
) where

import Prelude hiding (head, tail, null)
import qualified Prelude (head, tail, null)

import QueueStore.Types.Job (Job(..))

import GHC.Generics

import qualified Data.Text as T
import qualified Data.Serialize as S

-- | Data type for the queues of tasks
data JobQueue = JobQueue { queue :: [Job] } deriving (Eq, Show, Generic)

-- For DB storage
instance S.Serialize JobQueue

--(<:>) :: Job -> JobQueue -> JobQueue
(<:>) job jobQueue = JobQueue $ job : (queue jobQueue)

head :: JobQueue -> Job
head = Prelude.head . queue

tail :: JobQueue -> JobQueue
tail = JobQueue . Prelude.tail . queue

null :: JobQueue -> Bool
null = Prelude.null . queue

newEmptyJobQueue :: JobQueue
newEmptyJobQueue = JobQueue []
