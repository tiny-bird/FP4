module Main where

import           Control.Exception
import           Database.HDBC
import           Database.HDBC.PostgreSQL (connectPostgreSQL)

import           Source
import           Author
import           TextInfo
import           SourceTerms

main = do
  c <- connectPostgreSQL "host=localhost dbname=postgres user=postgres password=Lisere77"
  
  putStrLn "Source start"
  allSources <- readAllSources c
  print allSources
  newSourceId <- createSource "source4" c
  print newSourceId
  newSource <- readSource c newSourceId
  print newSource
  updateSource newSourceId "source5" c
  updatedSource <- readSource c newSourceId
  print updatedSource
  successfullyDeletedSource <- deleteSource newSourceId c
  print successfullyDeletedSource
  emptySource <- readSource c newSourceId
  print emptySource
  newSourceId <- createSource "source6" c
  putStrLn "Source end"
  
  
  putStrLn "Author start"
  allAuthors <- readAllAuthors c
  print allAuthors
  newAuthorId <- createAuthor "Olexiy" "Lyaschenko" newSourceId c
  print newAuthorId
  newAuthor <- readAuthor c newAuthorId
  print newAuthor
  updateAuthor newAuthorId "Olexiy1" "Lyaschenko1" newSourceId c
  updatedAuthor <- readAuthor c newAuthorId
  print updatedAuthor
  successfullyDeletedAuthor <- deleteAuthor newAuthorId c
  print successfullyDeletedAuthor
  emptyAuthor <- readAuthor c newAuthorId
  print emptyAuthor
  newAuthorId <- createAuthor "Olexiy1" "Lyaschenko1" newSourceId c
  putStrLn "Author end"
  

  putStrLn "TextInfo start"
  allTextInfo <- readAllTextInfo c
  print allTextInfo
  newTextInfoId <- createTextInfo "about dogs" "There were some dogs" newSourceId c
  print newTextInfoId
  newTextInfo <- readTextInfo c newTextInfoId
  print newTextInfo
  updateTextInfo newTextInfoId "pampam" "..." newSourceId c
  updatedTextInfo <- readTextInfo c newTextInfoId
  print updatedTextInfo
  successfullyDeletedTextInfo <- deleteTextInfo newTextInfoId c
  print successfullyDeletedTextInfo
  emptyTextInfo <- readTextInfo c newTextInfoId
  print emptyTextInfo
  putStrLn "TextInfo end"
  
  putStrLn "SourceTerms"
  allSourceTerms <- readAllSourceTerms c
  print allSourceTerms
  newSourceTermsId <- createSourceTerms "Rule3..." "parampampam" newSourceId c
  print newSourceTermsId
  newSourceTerms <- readSourceTerms c newSourceTermsId
  print newSourceTerms
  updateSourceTerms newSourceTermsId "Rule4..." "parampampam2" newSourceId c
  updatedSourceTerms <- readSourceTerms c newSourceTermsId
  print updatedSourceTerms
  successfullyDeletedSourceTerms <- deleteSourceTerms newSourceTermsId c
  print successfullyDeletedSourceTerms
  emptySourceTerms <- readSourceTerms c newSourceTermsId
  print emptySourceTerms
  putStrLn "SourceTerms"
