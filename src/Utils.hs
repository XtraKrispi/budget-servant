module Utils where

import           Data.Char                      ( toLower
                                                , toUpper
                                                )

convertToCamelCase :: String -> String
convertToCamelCase []       = []
convertToCamelCase (c : cs) = toLower c : cs

convertToUpperCase :: String -> String
convertToUpperCase []       = []
convertToUpperCase (c : cs) = toUpper c : cs
