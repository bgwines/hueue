{-# LANGUAGE OverloadedStrings #-}

module GithubWebhook.Constants
( githubEvent
) where

import qualified Data.Text.Lazy as TL

type Error = String

githubEvent :: TL.Text
githubEvent = "X-GitHub-Event"
