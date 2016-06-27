module Utils
( senderJSONFieldModifier
) where

import qualified Data.Char as Ch

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