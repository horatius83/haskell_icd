{-# LANGUAGE OverloadedStrings #-}
module Icd10Queries 
( create
, createTable
, retrieve
, retrieveIfTableExists
)
where

import Icd10Codes (Icd10CmPcsOrder(..))
import Database.SQLite.Simple (Query, Connection, execute, execute_, query, query_, Only(..), field, ToRow(..))
import Database.SQLite.Simple.FromRow (FromRow(..))

-- I can't seem to query data for primitive types, so here's a wrapper around String
data TableName = TableName String deriving (Show, Eq)

instance FromRow TableName where
    fromRow = TableName <$> field

create :: Connection -> Icd10CmPcsOrder -> IO ()
create conn icd = do
    let 
        q = "INSERT INTO Icd10CmPcsOrder (orderNumber, code, isHeader, shortDescription, longDescription)\n\
\VALUES (?, ?, ?, ?, ?)" :: Query
    execute conn q (icd)

createTable :: Connection -> IO ()
createTable conn = do
    let 
        q = "CREATE TABLE IF NOT EXISTS Icd10CmPcsOrder ( \n\
      \id INTEGER PRIMARY KEY\n\
    \, orderNumber INTEGER NOT NULL\n\
    \, code TEXT NOT NULL\n\
    \, isHeader NUMERIC NOT NULL\n\
    \, shortDescription TEXT NOT NULL\n\
    \, longDescription TEXT NOT NULL\n\
\)" :: Query
    execute_ conn q


retrieve :: Connection -> Int -> IO [Icd10CmPcsOrder]
retrieve conn id = do
    let 
        q = "SELECT\n\
      \id\n\
    \, orderNumber\n\
    \, code\n\
    \, isHeader\n\
    \, shortDescription\n\
    \, longDescription\n\
\FROM\n\
\    Icd10CmPcsOrder\n\
\WHERE\n\
\    id = ?" :: Query
    r <- query conn q (Only (id)) :: IO [Icd10CmPcsOrder]
    return r

retrieveIfTableExists :: Connection -> IO Bool
retrieveIfTableExists conn = do
    let
        q = "SELECT\n\
    \name\n\
\FROM\n\
    \sqlite_schema\n\
\WHERE\n\
    \type = 'table'\n\
    \AND name = 'Icd10CmPcsOrder'" :: Query
    results <- query_ conn q :: IO [TableName]
    let
        doesTableExist = length results > 0
    return doesTableExist