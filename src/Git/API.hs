{-# LANGUAGE OverloadedStrings #-}

module Git.API
( clone
, checkout
, merge
, mergeNoFF
, push
, forcePush
, rebase
) where

import qualified Data.Text as T

import qualified Shelly

clone :: T.Text -> IO ()
clone repoName = Shelly.shelly $ do
    Shelly.run_ "git" ["clone", repoName]

checkout :: T.Text -> IO ()
checkout branchName = Shelly.shelly $ do
    Shelly.run_ "git" ["checkout", branchName]

merge :: T.Text -> IO ()
merge branchName = Shelly.shelly $ do
    Shelly.run_ "git" ["merge", branchName]

mergeNoFF :: T.Text -> IO ()
mergeNoFF branchName = Shelly.shelly $ do
    Shelly.run_ "git" ["merge", "--no-ff", branchName]

push :: IO ()
push = Shelly.shelly $ do
    Shelly.run_ "git" ["push"]

forcePush :: IO ()
forcePush = Shelly.shelly $ do
    Shelly.run_ "git" ["push", "-f"]

rebase :: T.Text -> IO ()
rebase branchName = Shelly.shelly $ do
    Shelly.run_ "git" ["rebase", "-p", branchName]
