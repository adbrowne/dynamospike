module AesonConfig
    (
        dynamoAesonOptions,
        dynamoAesonOptionsDropPrefix
    ) where

import qualified Data.Char as Char
import Data.Aeson.TH
import Data.List

pascalCase :: String -> String
pascalCase [] = []
pascalCase (x:xs) = (Char.toUpper x):xs

dropPrefix :: String -> String -> String
dropPrefix prefix a =
    if isPrefixOf prefix a then
        drop (length prefix) a
    else
        a

dynamoAesonOptions = 
     defaultOptions { fieldLabelModifier = pascalCase }

dynamoAesonOptionsDropPrefix prefix = 
     defaultOptions { fieldLabelModifier = pascalCase . (dropPrefix prefix) }
