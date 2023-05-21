module Main (main) where

import Database.SQLite.Simple (open, close)
import Icd10Queries (retrieveIfTableExists)
import Icd10Codes (getIcd10CodesFromFile)
import Data.Either (isLeft, fromLeft, fromRight)

main :: IO ()
main = do 
    putStrLn "Opening connection..."
    conn <- open "test.db"
    putStrLn "Connection opened."
    haveTablesBeenCreated <- retrieveIfTableExists conn
    if not haveTablesBeenCreated then do
        let textFile =  "./data/icd10/icd10cm_order_2023.txt"
        putStrLn $ "Tables have not been created, reading codes from " ++ textFile
        eitherIcd10Codes <- getIcd10CodesFromFile textFile
        if isLeft eitherIcd10Codes then putStrLn (fromLeft "" eitherIcd10Codes)
        else do
            let
                icd10Codes = fromRight [] eitherIcd10Codes
                nIcd10Codes = show $ length icd10Codes
            putStrLn $ nIcd10Codes ++ " codes found."
    else putStrLn "Tables have been created"
    putStrLn "Closing connection."
    close conn
