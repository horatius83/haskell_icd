module Main
  ( main,
  )
where

import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT)
import Data.Either (fromLeft, fromRight, isLeft)
import Data.Text (Text, pack, strip, unpack)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
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
      Left error -> putStrLn error
  putStrLn "Closing connection."

insertCodesIntoDatabase :: Text -> [Icd10CmPcsOrder] -> IO ()
insertCodesIntoDatabase dbFilePath icd10Codes = runSqlite dbFilePath $ do
  runMigration migrateAll
  insertMany_ icd10Codes
