{-# LANGUAGE OverloadedStrings #-}

module GithubWebhook (main) where

import qualified Web.Scotty as Scotty
import qualified Network.Wai as Wai

import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy.Internal as BSLI

import GHC.Generics

import Data.Monoid
import Control.Monad
import Control.Applicative
import Control.Monad.IO.Class

import qualified Data.Aeson as A

import GithubWebhook.Types.Events.PushEvent
import GithubWebhook.Types.Events.IssueCommentEvent
import qualified Utils as U

main :: IO ()
main = serve 4567

serve :: Int -> IO ()
serve port = Scotty.scotty port $ do
    Scotty.post "/payload" $ do
        request <- Scotty.request
        headers <- Scotty.headers
        body <- Scotty.body
        U.printIO request
        U.printIO headers
        U.printIO body
        handleGithubWebrequest request headers body

    Scotty.notFound $ do
        Scotty.text "there is no such route."

handleGithubWebrequest :: Wai.Request -> [(TL.Text, TL.Text)] -> BSLI.ByteString -> Scotty.ActionM ()
handleGithubWebrequest request headers body = do
    let maybeEvent = fmap snd . L.find ((==) "X-GitHub-Event" . fst) $ headers
    case maybeEvent of
        Just "push" -> handlePush request headers body
        Just "issue_comment" -> handleIssueComment request headers body
        _ -> U.putStrLnIO $ "Cannot handle Github event: " ++ (show maybeEvent)

handleIssueComment request headers body = do
    U.putStrLnIO $ "Handling an issue comment!"
    U.printIO $ (A.eitherDecode body :: Either String IssueCommentEvent)

handlePush request headers body = do
    U.putStrLnIO $ "Handling a push!"
    U.printIO $ (A.eitherDecode body :: Either String PushEvent)
