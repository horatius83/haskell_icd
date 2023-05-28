{-# LANGUAGE OverloadedStrings          #-}

module Main
  ( main,
  )
where

import Control.Monad (unless)
import qualified Data.Text as T
import Database.Persist
import Database.Persist.Sqlite
import Icd10Codes (Icd10CmPcsOrder(..), getIcd10CodesFromFile, migrateAll, EntityField(..))
import System.Directory (doesFileExist)
import System.Environment (getArgs)

main :: IO ()
main = do
  (databasePath, cmsFilePath) <- getSourceFiles "icd10Codes.db" "./data/icd10/icd10cm_order_2023.txt"
  putStrLn $ "Checking if " ++ databasePath ++ " exists..."
  doesDbFileExist <- doesFileExist databasePath
  unless doesDbFileExist $ do
    putStrLn $ "No database found, parsing CMS file from " ++ cmsFilePath
    eitherIcdCodes <- getIcd10CodesFromFile cmsFilePath
    putStrLn "Inserting into database..."
    case eitherIcdCodes of
      Right icdCodes -> insertCodesIntoDatabase (T.pack databasePath) icdCodes
      Left e -> putStrLn e
    putStrLn "Database created."
  icd10LookupLoop $ T.pack databasePath

icd10LookupLoop :: T.Text -> IO ()
icd10LookupLoop dbPath = do
  putStrLn "Enter ICD 10 code to lookup (or quit): "
  response <- getLine
  case response of
    "quit" -> return ()
    _ -> do
      putStrLn $ "Looking up "++ response
      results <- icd10Lookup dbPath $ T.pack response
      mapM_ print results
      icd10LookupLoop dbPath

-- https://stackoverflow.com/questions/11048143/example-of-persistent-with-backend-specific-operator
icd10Lookup :: T.Text -> T.Text -> IO [Entity Icd10CmPcsOrder]
icd10Lookup databasePath lookupValue = runSqlite databasePath $ do
  let 
    searchText = T.concat ["%", lookupValue, "%"]
    fv = FilterValue searchText
  selectList [Filter Icd10CmPcsOrderLongDescription fv (BackendSpecificFilter "like")] []

getSourceFiles :: String -> String -> IO (String, String)
getSourceFiles defaultDatabasePath defaultCmsFilePath = do
  args <- getArgs
  return $ case args of
    (databasePath : cmsFilePath : _) -> (databasePath, cmsFilePath)
    (databaseFilePath : _) -> (databaseFilePath, defaultCmsFilePath)
    _ -> (defaultDatabasePath, defaultCmsFilePath)

insertCodesIntoDatabase :: T.Text -> [Icd10CmPcsOrder] -> IO ()
insertCodesIntoDatabase dbFilePath icd10Codes = runSqlite dbFilePath $ do
  runMigration migrateAll
  insertMany_ icd10Codes
