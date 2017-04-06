{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Job where

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
|]

instance ToJSON Job where
    toJSON (Job repoID srcBranch dstBranch) =
        object ["repoID" .= repoID, "srcBranch" .= srcBranch, "dstBranch" .= dstBranch]

