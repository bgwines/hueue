{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module DataStore.Token where

import Database.Persist
import Database.Persist.TH
import Database.Persist.Sqlite

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
OAuth2Token
    userID Int
    token String
    UniqueUserID userID
    deriving Show
|]
