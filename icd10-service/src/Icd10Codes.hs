{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Icd10Codes
  ( Icd10CmPcsOrder(..)
  , parseIcd10CmOrder
  , parseIcd10CmOrders
  , getIcd10CodesFromFile
  , migrateAll
  , EntityField(..)
  ) where

import           Data.Text               (Text, pack, strip)
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           Text.Read               (readMaybe)

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Icd10CmPcsOrder
    orderNumber Int
    code Text
    isHeader Bool
    shortDescription Text
    longDescription Text
    deriving Show Eq
|]

{-
ICD-10-CM/PCS Order File Format
Position    Length  Contents
1           5       Order number, right justified, zero filled.
6           1       Blank
7           7       ICD-10-CM or ICD-10-PCS code. Dots are not included.
14          1       Blank
15          1       0 if the code is a “header” –not valid for HIPAA-covered transactions. 1 if the code is valid for submission for HIPAA-covered transactions.
16          1       Blank
17          60      Short description
77          1       Blank
78          To end  Long description
-}
parseIcd10CmOrder :: String -> Maybe Icd10CmPcsOrder
parseIcd10CmOrder line =
  let substring index charCount = take charCount $ drop index line
      parsedOrderNumber = readMaybe $ substring 0 5
      parse = strip . pack
      parsedCode = substring 6 7
      parsedHeader = substring 14 1
      parsedIsHeader
        | parsedHeader == "0" = False
        | otherwise = True
      parsedShortDescription = substring 16 60
      parsedLongDescription = substring 77 (length line - 77)
      makeIcd justOrderNumber =
        Icd10CmPcsOrder
          justOrderNumber
          (parse parsedCode)
          parsedIsHeader
          (parse parsedShortDescription)
          (parse parsedLongDescription)
   in fmap makeIcd parsedOrderNumber

parseIcd10CmOrders :: [String] -> Either String [Icd10CmPcsOrder]
parseIcd10CmOrders textLines =
  let parse ln =
        let order = parseIcd10CmOrder ln
         in case order of
              Just (x) -> Right x
              Nothing  -> Left $ "Error parsing: " ++ ln
   in mapM parse textLines

getIcd10CodesFromFile :: String -> IO (Either String [Icd10CmPcsOrder])
getIcd10CodesFromFile file = do
  contents <- readFile file
  let lines' = lines contents
      codes = parseIcd10CmOrders lines'
  return codes
