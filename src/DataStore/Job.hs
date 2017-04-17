{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module DataStore.Job where

import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import GHC.Generics
import DataStore.JobState
import qualified Data.Text as T

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Job
    repoID Int
    srcBranch T.Text
    dstBranch T.Text
    state JobState
    deriving Show
    deriving Generic
|]
