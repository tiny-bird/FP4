module Source where

import           Common
import qualified Data.ByteString.Char8    as BS
import           Data.List
import           Database.HDBC
import           Database.HDBC.PostgreSQL
import           Prelude                  hiding (read)

type Id = Integer

type Title = String


unpack [SqlInteger uid, SqlByteString title] = (uid, BS.unpack title)
unpack x = error $ "Unexpected result: " ++ show x

createSource :: IConnection a => Title -> a -> IO Integer
createSource title conn = withTransaction conn (create' title)

create' title conn = do
  changed <- run conn query [SqlString title]
  result <- quickQuery' conn lastId []
  let rows = map Common.convRow result
  return $ last rows
  where
    query = "insert into source (title) values (?)"
    lastId = "select max(id) from source"

readSource :: IConnection a => a -> Id -> IO (Id, Title)
readSource conn id = do
  result <- quickQuery' conn query [SqlInteger id]
  let rows = map unpack result
  if null rows
    then return (-1, "")
    else return $ last rows
  where
    query = "select * from source where id = ?"

readAllSources :: IConnection a => a -> IO [(Id, Title)]
readAllSources conn = do
  result <- quickQuery' conn query []
  return $ map unpack result
  where
    query = "select * from source order by id"

updateSource :: IConnection a => Id -> Title -> a -> IO (Id, Title)
updateSource uid title conn = withTransaction conn (update' uid title )

update' uid title conn = do
  changed <- run conn query [SqlString title, SqlInteger uid]
  result <- quickQuery' conn newValue [SqlInteger uid]
  let rows = map unpack result
  return $ last rows
  where
    query = "update source set title = ? where id = ?"
    newValue = "select id, title from source where id = ?"

deleteSource :: IConnection a => Id -> a -> IO Bool
deleteSource id conn = withTransaction conn (delete' id)

delete' id conn = do
  changed <- run conn query [SqlInteger id]
  return $ changed == 1
  where
    query = "delete from source where id = ?"

deleteAllSources :: IConnection a => a -> IO Bool
deleteAllSources conn = withTransaction conn deleteAll'

deleteAll' conn = do
  changed <- run conn query []
  return $ changed == 1
  where
    query = "delete from source"
