{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module DataStore.JobState (JobState(..)) where

import GHC.Generics
import Database.Persist
import Database.Persist.TH
import Database.Persist.Sqlite

data JobState = Waiting | Running | Killed deriving (Show, Read, Eq, Generic)
derivePersistField "JobState"
