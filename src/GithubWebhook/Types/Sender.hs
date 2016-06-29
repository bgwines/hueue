{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module GithubWebhook.Types.Sender
( Sender(..)
) where

import qualified Data.Char as Ch
import qualified Data.Text as T
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A

import GHC.Generics

import qualified Utils

data Sender = Sender
    { login :: T.Text
    , id :: Integer
    , avatarUrl :: T.Text
    , gravatarId :: T.Text
    , url :: T.Text
    , htmlUrl :: T.Text
    , followersUrl :: T.Text
    , followingUrl :: T.Text
    , gistsUrl :: T.Text
    , starredUrl :: T.Text
    , subscriptionsUrl :: T.Text
    , organizationsUrl :: T.Text
    , reposUrl :: T.Text
    , eventsUrl :: T.Text
    , receivedEventsUrl :: T.Text
    , senderType :: T.Text
    , siteAdmin :: Bool } deriving (Eq, Generic, Show)

$(A.deriveJSON
    A.defaultOptions
    {A.fieldLabelModifier = Utils.senderJSONFieldModifier}
    ''Sender)
