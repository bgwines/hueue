{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module GithubWebhook.Types.Comment
( Comment(..)
) where

import qualified Data.Char as Ch
import qualified Data.Text as T
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A

import GHC.Generics

import qualified Utils

import qualified GithubWebhook.Types.SmallUser as SmallUser

data Comment = Comment
    { url :: T.Text
    , htmlUrl :: T.Text
    , issueUrl :: T.Text
    , id :: Integer
    , user :: SmallUser.SmallUser
    , createdAt :: T.Text
    , updatedAt :: T.Text
    , body :: T.Text
    } deriving (Eq, Generic, Show)

$(A.deriveJSON
    A.defaultOptions
    {A.fieldLabelModifier = Utils.camelCaseToSnakeCase}
    ''Comment)
