module TextInfo where

import           Common
import qualified Data.ByteString.Char8    as BS
import           Data.List
import           Database.HDBC
import           Database.HDBC.PostgreSQL
import           Prelude                  hiding (read)

type Id = Integer

type TextType = String

type Text = String

unpack [SqlInteger uid, SqlByteString textType, SqlByteString text, SqlInteger tid] = (uid, BS.unpack textType, BS.unpack text, tid)
unpack x = error $ "Unexpected result: " ++ show x

createTextInfo :: IConnection a => TextType -> Text -> Id -> a -> IO Integer
createTextInfo textType text tid conn = withTransaction conn (create' textType text tid)

create' textType text tid conn = do
  changed <- run conn query [SqlString textType, SqlString text, SqlInteger tid]
  result <- quickQuery' conn lastId []
  let rows = map Common.convRow result
  return $ last rows
  where
    query = "insert into text_info (type, text, sourceId) values (?, ?, ?)"
    lastId = "select max(id) from text_info"

readTextInfo :: IConnection a => a -> Id -> IO (Id, TextType, Text, Id)
readTextInfo conn id = do
  result <- quickQuery' conn query [SqlInteger id]
  let rows = map unpack result
  if null rows
    then return (-1, "", "", -1)
    else return $ last rows
  where
    query = "select * from text_info where id = ?"

readAllTextInfo :: IConnection a => a -> IO [(Id, TextType, Text, Id)]
readAllTextInfo conn = do
  result <- quickQuery' conn query []
  return $ map unpack result
  where
    query = "select * from text_info order by id"

updateTextInfo :: IConnection a => Id -> TextType -> Text -> Id -> a -> IO (Id, TextType, Text, Id)
updateTextInfo uid textType text tid conn = withTransaction conn (update' uid textType text tid)

update' uid textType text tid conn = do
  changed <- run conn query [SqlString textType, SqlString text, SqlInteger tid, SqlInteger uid]
  result <- quickQuery' conn newValue [SqlInteger uid]
  let rows = map unpack result
  return $ last rows
  where
    query = "update text_info set type = ?, text = ?, sourceId = ? where id = ?"
    newValue = "select id, type, text, sourceId from text_info where id = ?"

deleteTextInfo :: IConnection a => Id -> a -> IO Bool
deleteTextInfo id conn = withTransaction conn (delete' id)

delete' id conn = do
  changed <- run conn query [SqlInteger id]
  return $ changed == 1
  where
    query = "delete from text_info where id = ?"

deleteAllTextInfo :: IConnection a => a -> IO Bool
deleteAllTextInfo conn = withTransaction conn deleteAll'

deleteAll' conn = do
  changed <- run conn query []
  return $ changed == 1
  where
    query = "delete from text_info"
