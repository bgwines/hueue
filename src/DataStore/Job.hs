{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric              #-}

module DataStore.Job where

import Data.Aeson
import GHC.Generics

import Database.Persist
import Database.Persist.TH
import Database.Persist.Sqlite

import qualified Data.Text as T

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Job
    repoID Int
    srcBranch T.Text
    dstBranch T.Text
    deriving Generic
    deriving Show
|]

instance ToJSON Job where
    toJSON (Job repoID srcBranch dstBranch) =
        object ["repoID" .= repoID, "srcBranch" .= srcBranch, "dstBranch" .= dstBranch]
