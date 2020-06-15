module SourceTerms where

import           Common
import qualified Data.ByteString.Char8    as BS
import           Data.List
import           Database.HDBC
import           Database.HDBC.PostgreSQL
import           Prelude                  hiding (read)

type Id = Integer

type Terms = String

type Purpose = String


unpack [SqlInteger uid, SqlByteString terms, SqlByteString purpose, SqlInteger tid] = (uid, BS.unpack terms, BS.unpack purpose, tid)
unpack x = error $ "Unexpected result: " ++ show x

createSourceTerms :: IConnection a => Terms -> Purpose ->  Id -> a -> IO Integer
createSourceTerms terms purpose tid conn = withTransaction conn (create' terms purpose tid)

create' terms purpose tid conn = do
  changed <- run conn query [SqlString terms, SqlString purpose, SqlInteger tid]
  result <- quickQuery' conn lastId []
  let rows = map Common.convRow result
  return $ last rows
  where
    query = "insert into source_terms (terms, purpose, sourceId) values (?, ?, ?)"
    lastId = "select max(id) from source_terms"

readSourceTerms :: IConnection a => a -> Id -> IO (Id, Terms, Purpose, Id)
readSourceTerms conn id = do
  result <- quickQuery' conn query [SqlInteger id]
  let rows = map unpack result
  if null rows
    then return (-1, "", "", -1)
    else return $ last rows
  where
    query = "select * from source_terms where id = ?"

readAllSourceTerms :: IConnection a => a -> IO [(Id, Terms, Purpose, Id)]
readAllSourceTerms conn = do
  result <- quickQuery' conn query []
  return $ map unpack result
  where
    query = "select * from source_terms order by id"

updateSourceTerms :: IConnection a => Id -> Terms -> Purpose ->  Id -> a -> IO (Id, Terms, Purpose, Id)
updateSourceTerms uid terms purpose tid conn = withTransaction conn (update' uid terms purpose tid)

update' uid terms purpose tid conn = do
  changed <- run conn query [SqlString terms, SqlString purpose, SqlInteger tid, SqlInteger uid]
  result <- quickQuery' conn newValue [SqlInteger uid]
  let rows = map unpack result
  return $ last rows
  where
    query = "update source_terms set terms = ?, purpose = ?,  sourceId = ? where id = ?"
    newValue = "select id, terms, purpose, sourceId from source_terms where id = ?"

deleteSourceTerms :: IConnection a => Id -> a -> IO Bool
deleteSourceTerms id conn = withTransaction conn (delete' id)

delete' id conn = do
  changed <- run conn query [SqlInteger id]
  return $ changed == 1
  where
    query = "delete from source_terms where id = ?"

deleteAllSourceTerms :: IConnection a => a -> IO Bool
deleteAllSourceTerms conn = withTransaction conn deleteAll'

deleteAll' conn = do
  changed <- run conn query []
  return $ changed == 1
  where
    query = "delete from soure_terms"
