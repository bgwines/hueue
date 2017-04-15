{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module DataStore.Repo where

import Database.Persist
import Database.Persist.TH
import Database.Persist.Sqlite

import qualified Data.Text as T

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Repo
    repoID Int
    mergerGithubUserID Int
    deriving Show
|]
