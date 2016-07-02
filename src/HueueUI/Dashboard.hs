{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

import Yesod

import Control.Monad.IO.Class
import Control.Monad.Trans.Either

import qualified QueueStore.Store
import qualified QueueStore.Types.JobQueue

data HueueUI = HueueUI
mkYesod "HueueUI" [parseRoutes|
/ HomeR GET
|]
instance Yesod HueueUI

eTqueue :: EitherT String IO QueueStore.Types.JobQueue.JobQueue
eTqueue = QueueStore.Store.loadQueueDEBUG 61999075

prettyPrintQueue :: QueueStore.Types.JobQueue.JobQueue -> String
prettyPrintQueue queue = show queue

getHomeR = defaultLayout $ do
    setTitle "Hueue dashboard"
    --toWidget [lucius| h1 { color: green; } |]
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
    equeue <- liftIO . runEitherT $ eTqueue
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

main = warp 3000 HueueUI
