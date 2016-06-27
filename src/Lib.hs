{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import qualified GitHub
import qualified GitHub.Auth as GitHub
import qualified GitHub.Endpoints.Users as GitHub
import qualified GitHub.Endpoints.Users.Followers as Github

someFunc :: IO ()
someFunc = do
    let name = GitHub.mkName undefined "bgwines"
    let possibleUsersRequest = GitHub.userInfoForR name
    let auth = GitHub.BasicAuth "bgwines" "Ginco142857"
    possibleUsers <- GitHub.executeRequest auth possibleUsersRequest
    print possibleUsers
    return ()
