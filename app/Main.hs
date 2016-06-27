{-# LANGUAGE OverloadedStrings #-}

module Main where

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

import qualified Events

main :: IO ()
main = Scotty.scotty 4567 $ do
    Scotty.post "/payload" $ do
        request <- Scotty.request
        headers <- Scotty.headers
        body <- Scotty.body
        liftIO . putStrLn $ "--------------------------"
        liftIO . print    $ request
        liftIO . putStrLn $ "--------------------------"
        liftIO . print    $ headers
        liftIO . putStrLn $ "--------------------------"
        liftIO . print    $ body
        handleGithubWebrequest request headers body

    Scotty.post "/:word" $ do
        beam <- Scotty.param "word"
        liftIO $ print $ mconcat ["get; word = ", beam]
        Scotty.html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]

    Scotty.notFound $ do
        Scotty.text "there is no such route."

handleGithubWebrequest :: Wai.Request -> [(TL.Text, TL.Text)] -> BSLI.ByteString -> Scotty.ActionM ()
handleGithubWebrequest request headers body = do
    let maybeEvent = fmap snd . L.find ((==) "X-GitHub-Event" . fst) $ headers
    case maybeEvent of
        Just "push" -> handlePush request headers body
        _ -> liftIO . print $ "Cannot handle Github event: " ++ (show maybeEvent)

handlePush request headers body = do
    liftIO . putStrLn $ "Handling a push!"
    liftIO . print $ (A.eitherDecode body :: Either String Events.PushEvent)
