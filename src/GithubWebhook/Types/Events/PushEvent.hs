{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module GithubWebhook.Types.Events.PushEvent
( PushEvent(..)
) where

import qualified Data.Text as T
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A

import qualified GithubWebhook.Types.SmallUser as SmallUser
import qualified GithubWebhook.Types.Repo as R
import qualified GithubWebhook.Types.Commit as C
import qualified GithubWebhook.Types.BigUser as BigUser

import GHC.Generics

import qualified Utils

data PushEvent = PushEvent
    { ref :: T.Text
    , before :: T.Text
    , after :: T.Text
    , created :: Bool
    , deleted :: Bool
    , forced :: Bool
    , baseRef :: Maybe T.Text
    , compare :: T.Text
    , commits :: [C.Commit]
    , headCommit :: C.Commit
    , repository :: R.Repo
    , pusher :: SmallUser.SmallUser
    , sender :: BigUser.BigUser } deriving (Eq, Generic, Show)

$(A.deriveJSON
    A.defaultOptions
    {A.fieldLabelModifier = Utils.camelCaseToSnakeCase}
    ''PushEvent)