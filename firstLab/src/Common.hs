module Common where

import           Database.HDBC
import           Database.HDBC.PostgreSQL

convRow :: [SqlValue] -> Integer
convRow [sqlId] = intId
  where
    intId = fromSql sqlId :: Integer
convRow x = error $ "Unexpected result: " ++ show x

getSourceId :: (a, b) -> a
getSourceId (x, _) = x

getSourceTitle :: (a, b) -> b
getSourceTitle (_, y) = y
