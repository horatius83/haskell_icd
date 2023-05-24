module Main
  ( main
  ) where

import           Control.Monad.IO.Class  (liftIO)
import           Data.Either             (fromLeft, fromRight, isLeft)
import           Data.Text               (Text, pack, strip)
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           Icd10Codes              (getIcd10CodesFromFile, migrateAll)

main :: IO ()
main = do
  putStrLn "Opening connection..."
  let databasePath = pack "test2.db"
      cmsFilePath = pack "./data/icd10/icd10cm_order_2023.txt"
  loadDatabase databasePath cmsFilePath
  putStrLn "Closing connection."

loadDatabase :: Text -> Text -> IO ()
loadDatabase dbFilePath cmsDataFilePath =
  runSqlite dbFilePath $ do 
    runMigration migrateAll
    count <- rawQuery "select count(1) from Icd10CmPcsOrder" []
    if count == 0 then do
        let insertIcd10Codes icd10Codes = do 
                putStrLn "Inserting codes into the database..."
                let icd10Codes = fromRight [] eitherIcd10Codes
                insertMany $ icd10Codes 
                putStrLn "Finished inserts."
        eitherIcd10Codes <- getIcd10CodesFromFile textFileLocation
        insertResults <- insertIcd10Codes <$> eitherIcd10Codes

        if isLeft eitherIcd10Codes then do 
            putStrLn (fromLeft "" eitherIcd10Codes)
        else do
            putStrLn "Inserting codes into the database..."
            let icd10Codes = fromRight [] eitherIcd10Codes
            insertMany $ icd10Codes 
            putStrLn "Finished inserts."
    else
        putStrLn "Else"
    putStrLn "Done"
