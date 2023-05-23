module Main (main) where

import           Control.Monad.IO.Class  (liftIO)
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import Icd10Codes (getIcd10CodesFromFile, migrateAll)
import Data.Either (isLeft, fromLeft, fromRight)
import Data.Text (Text, pack, strip)

main :: IO ()
main = do 
    putStrLn "Opening connection..."
    let
        databasePath = pack "test.db" 
        cmsFilePath = pack "./data/icd10/icd10cm_order_2023.txt"
    loadDatabase databasePath cmsFilePath
    putStrLn "Closing connection."

loadDatabase :: Text -> Text -> IO ()
loadDatabase dbFilePath cmsDataFilePath = runSqlite dbFilePath $ do
    runMigration migrateAll
    -- get row count
    -- if it is zero then 
        -- parse values from file
        -- insert into database

{-
loadDatabase :: Connection -> String -> IO ()
loadDatabase conn textFileLocation = do
    haveTablesBeenCreated <- retrieveIfTableExists conn
    if not haveTablesBeenCreated then do
        putStrLn $ "Tables have not been created, reading codes from " ++ textFileLocation
        eitherIcd10Codes <- getIcd10CodesFromFile textFileLocation
        if isLeft eitherIcd10Codes then putStrLn (fromLeft "" eitherIcd10Codes)
        else do
            let
                icd10Codes = fromRight [] eitherIcd10Codes
                nIcd10Codes = show $ length icd10Codes
                createIcd = create conn
            putStrLn $ nIcd10Codes ++ " codes found."
            putStrLn "Creating table"
            createTable conn
            putStrLn "Inserting values"
            mapM_ createIcd icd10Codes
            putStrLn "Values inserted"
    else putStrLn "Tables have been created"
-}

