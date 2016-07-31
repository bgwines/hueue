{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module HueueUI.Dashboard (main) where

import Yesod

import Utils
import MonadImports

import qualified QueueStore.Store
import qualified QueueStore.Types.JobQueue

data HueueUI = HueueUI

mkYesod "HueueUI" [parseRoutes|
/ HomeR GET
/clearQueue ClearQueueR GET
/oauthRedirect OAuthRedirectR GET
|]
instance Yesod HueueUI

prettyPrintQueue :: QueueStore.Types.JobQueue.JobQueue -> String
prettyPrintQueue queue = show queue

getOAuthRedirectR = defaultLayout $ do
    toWidget
        [hamlet|
            <h1>"hello O____O"
            |]

getClearQueueR = defaultLayout $ do
    clearResult <- liftIO . runEitherT $ QueueStore.Store.clearQueueDEBUG 61999075
    toWidget
        [hamlet|
            <h2>Clearing the repo's jobqueue
            $case clearResult
                $of Left msg
                    <p>Error when clearing queue; failed with error message #{msg}
                $of Right ()
                    <p>Cleared successfully!
        |]

getHomeR = defaultLayout $ do
    setTitle "Hueue dashboard"
    toWidgetHead [hamlet|<h1>Hueue dashboard (powered by HueueUI! :o)|]
    addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.6.2/jquery.min.js"
    toWidget
        [julius|
            $(function() {
                $("h1").click(function(){
                    alert("You clicked on the heading!");
                });
            });
        |]
    equeue <- liftIO . runEitherT $ QueueStore.Store.loadQueueDEBUG 61999075
    toWidget
        [hamlet|
            <h2>Jobs:
            $case equeue
                $of Left msg
                    <p>Error when loading jobs; failed with error message #{msg}
                $of Right queue
                    $if QueueStore.Types.JobQueue.null queue
                        <p>You don't have any jobs.
                    $else
                        <ul>
                            $forall job <- QueueStore.Types.JobQueue.queue queue
                                <li>#{show job}
        |]
    let oauthURL = "https://github.com/login/oauth/authorize"
            ++ "?client_id="    ++ "416fdf5ed5fb66f16bd3" -- TODO: DB this up
            ++ "&redirect_uri=" ++ "localhost/oauthRedirect"
            ++ "&scope="        ++ "repo"
            ++ "&state="        ++ "142857" -- TODO: dynamically generate
            ++ "&allow_signup=" ++ "true" :: String
    toWidget
        [hamlet|
            <p><a href=#{oauthURL}>Give Hueue access
        |]

main = warp 3000 HueueUI
