module Main where

import System.Directory (getDirectoryContents)
import Data.String.Utils (startswith, endswith)
import Text.XML.HXT.Core
import Text.HandsomeSoup

apiFileFilter fileName =
  (endswith ".html" fileName) && (startswith "API" fileName)

apiDirectory = "./apiref/"

getTitle doc = do
  let titleSearch = doc >>> css "h1.topictitle" //> getText
  titles <- runX titleSearch
  return $ head titles

getSection doc = do
  putStrLn "Section"
  putStrLn . show $ doc
  -- blah <- runX . xshow $ doc
  -- (putStrLn . head) blah
  return ()
  
getSections doc = do
  let sectionSearch = doc 
        >>> css "div#divContent" 
        >>> (getChildren >>> css ".section")
        >>. tail
        >>> (css "h2.title" //> getText)
         -- >>> getText
  result <- runX sectionSearch
  mapM_ getSection $ result
  return "andrew"

-- getFileSpec :: Show a => String -> IO a
getFileSpec fileName = do 
  html <- readFile (apiDirectory ++ fileName)
  let doc = readString [withParseHTML yes, withWarnings no] html
  title <- getTitle doc
  sections <- getSections doc
  return sections

wholeDirectory = do
  directoryContents <- getDirectoryContents "./apiref/"
  let files = filter apiFileFilter directoryContents
  stuff <- mapM getFileSpec files
  mapM_ (putStrLn . show) stuff

main = do 
  sample <- getFileSpec "API_CreateTableResult.html"
  putStrLn . show $ sample
