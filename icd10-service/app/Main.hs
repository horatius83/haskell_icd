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

main :: IO ()
main = do
  putStrLn "Opening connection..."
  let databasePath = "test2.db"
      cmsFilePath = "./data/icd10/icd10cm_order_2023.txt"
  doesDbFileExist <- doesFileExist databasePath
  unless doesDbFileExist $ do
    eitherIcdCodes <- getIcd10CodesFromFile cmsFilePath
    case eitherIcdCodes of
      Right icdCodes -> insertCodesIntoDatabase (pack databasePath) icdCodes
      Left e -> putStrLn e
  putStrLn "Closing connection."

insertCodesIntoDatabase :: Text -> [Icd10CmPcsOrder] -> IO ()
insertCodesIntoDatabase dbFilePath icd10Codes = runSqlite dbFilePath $ do
  runMigration migrateAll
  insertMany_ icd10Codes
