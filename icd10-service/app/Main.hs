module Main (main) where

import Database.SQLite.Simple (open, close)
import Icd10Queries (retrieveIfTableExists)

main :: IO ()
main = do 
    putStrLn "Opening connection..."
    conn <- open "test.db"
    putStrLn "Connection opened."
    haveTablesBeenCreated <- retrieveIfTableExists conn
    if haveTablesBeenCreated then putStrLn "Tables have been created"
    else putStrLn "Tables have NOT been created"
    close conn