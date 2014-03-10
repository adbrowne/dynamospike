module AesonConfig
    (
        dynamoAesonOptions
    ) where

import qualified Data.Char as Char
import Data.Aeson.TH

pascalCase :: String -> String
pascalCase [] = []
pascalCase (x:xs) = (Char.toUpper x):xs

dynamoAesonOptions = 
     defaultOptions { fieldLabelModifier = pascalCase }

