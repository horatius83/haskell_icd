module Main
  ( main,
  )
where

import Control.Monad (unless)
import Data.Text (Text, pack)
import Database.Persist
import Database.Persist.Sqlite
import Icd10Codes (Icd10CmPcsOrder, getIcd10CodesFromFile, migrateAll)
import System.Directory (doesFileExist)
import System.Environment (getArgs)

main :: IO ()
main = do
  (databasePath, cmsFilePath) <- getSourceFiles "icd10Codes.db"  "./data/icd10/icd10cm_order_2023.txt"
  putStrLn $ "Checking if " ++ databasePath ++ " exists..."
  doesDbFileExist <- doesFileExist databasePath
  unless doesDbFileExist $ do
    putStrLn $ "No database found, parsing CMS file from " ++ cmsFilePath
    eitherIcdCodes <- getIcd10CodesFromFile cmsFilePath
    putStrLn "Inserting into database..."
    case eitherIcdCodes of
      Right icdCodes -> insertCodesIntoDatabase (pack databasePath) icdCodes
      Left e -> putStrLn e
    putStrLn "Database created."

getSourceFiles :: String -> String -> IO (String, String)
getSourceFiles defaultDatabasePath defaultCmsFilePath = do
    args <- getArgs
    return $ case args of
        (databasePath:cmsFilePath:_) -> (databasePath, cmsFilePath)
        (databaseFilePath:_) -> (databaseFilePath, defaultCmsFilePath)
        _ -> (defaultDatabasePath, defaultCmsFilePath)

insertCodesIntoDatabase :: Text -> [Icd10CmPcsOrder] -> IO ()
insertCodesIntoDatabase dbFilePath icd10Codes = runSqlite dbFilePath $ do
  runMigration migrateAll
  insertMany_ icd10Codes
