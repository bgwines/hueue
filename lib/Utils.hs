module Utils
( senderJSONFieldModifier
, printIO
, putStrLnIO
) where

import qualified Data.Char as Ch

import Control.Monad.IO.Class

camelCaseToSnakeCase :: String -> String
camelCaseToSnakeCase [] = []
camelCaseToSnakeCase (ch:s)
    | Ch.isUpper ch = '_' : Ch.toLower ch : camelCaseToSnakeCase s
    | otherwise = ch : camelCaseToSnakeCase s

senderJSONFieldModifier :: String -> String
senderJSONFieldModifier fieldName =
    case fieldName of
        "senderType" -> "type"
        _ -> camelCaseToSnakeCase fieldName

printIO :: (MonadIO m, Show a) => a -> m ()
printIO = liftIO . print

putStrLnIO :: (MonadIO m) => String -> m ()
putStrLnIO = liftIO . putStrLn