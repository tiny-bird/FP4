module Author where

import           Common
import qualified Data.ByteString.Char8    as BS
import           Data.List
import           Database.HDBC
import           Database.HDBC.PostgreSQL
import           Prelude                  hiding (read)

type Id = Integer

type Name = String

type Surname = String

unpack [SqlInteger uid, SqlByteString name, SqlByteString surname, SqlInteger tid] = (uid, BS.unpack name, BS.unpack surname, tid)
unpack x = error $ "Unexpected result: " ++ show x

createAuthor :: IConnection a => Name -> Surname -> Id -> a -> IO Integer
createAuthor name surname tid conn = withTransaction conn (create' name surname tid)

create' name surname tid conn = do
  changed <- run conn query [SqlString name, SqlString surname, SqlInteger tid]
  result <- quickQuery' conn lastId []
  let rows = map Common.convRow result
  return $ last rows
  where
    query = "insert into author (name, surname, sourceId) values (?, ?, ?)"
    lastId = "select max(id) from author"

readAuthor :: IConnection a => a -> Id -> IO (Id, Name, Surname, Id)
readAuthor conn id = do
  result <- quickQuery' conn query [SqlInteger id]
  let rows = map unpack result
  if null rows
    then return (-1, "", "", -1)
    else return $ last rows
  where
    query = "select * from author where id = ?"

readAllAuthors :: IConnection a => a -> IO [(Id, Name, Surname, Id)]
readAllAuthors conn = do
  result <- quickQuery' conn query []
  return $ map unpack result
  where
    query = "select * from author order by id"

updateAuthor :: IConnection a => Id -> Name -> Surname -> Id -> a -> IO (Id, Name, Surname, Id)
updateAuthor uid name surname tid conn = withTransaction conn (update' uid name surname tid)

update' uid name surname tid conn = do
  changed <- run conn query [SqlString name, SqlString surname, SqlInteger tid, SqlInteger uid]
  result <- quickQuery' conn newValue [SqlInteger uid]
  let rows = map unpack result
  return $ last rows
  where
    query = "update author set name = ?, surname = ?, sourceId = ? where id = ?"
    newValue = "select id, name, surname, sourceId from author where id = ?"

deleteAuthor :: IConnection a => Id -> a -> IO Bool
deleteAuthor id conn = withTransaction conn (delete' id)

delete' id conn = do
  changed <- run conn query [SqlInteger id]
  return $ changed == 1
  where
    query = "delete from author where id = ?"

deleteAllAuthors :: IConnection a => a -> IO Bool
deleteAllAuthors conn = withTransaction conn deleteAll'

deleteAll' conn = do
  changed <- run conn query []
  return $ changed == 1
  where
    query = "delete from author"
