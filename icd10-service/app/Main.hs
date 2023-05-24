module Main (main) where

import Data.Either (fromLeft, fromRight, isLeft)
import Icd10Codes (getIcd10CodesFromFile)

main :: IO ()
main = do
  putStrLn "Opening connection..."
  -- conn <- open "test.db"
  -- loadDatabase conn "./data/icd10/icd10cm_order_2023.txt"
  putStrLn "Closing connection."
  -- close conn

{-
loadDatabase :: Connection -> String -> IO ()
loadDatabase conn textFileLocation = do
  haveTablesBeenCreated <- retrieveIfTableExists conn
  if not haveTablesBeenCreated
    then do
      putStrLn $ "Tables have not been created, reading codes from " ++ textFileLocation
      eitherIcd10Codes <- getIcd10CodesFromFile textFileLocation
      if isLeft eitherIcd10Codes
        then putStrLn (fromLeft "" eitherIcd10Codes)
        else do
          let icd10Codes = fromRight [] eitherIcd10Codes
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