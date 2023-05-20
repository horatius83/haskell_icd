module Icd10Codes   
( Icd10CmPcsOrder(..)
, parseIcd10CmOrder
) where

import Data.Text (Text, pack)
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
data Icd10CmPcsOrder = Icd10CmPcsOrder  
    { orderNumber :: Int
    , code :: Text
    , isHeader :: Bool
    , shortDescription :: Text
    , longDescription :: Text
    } deriving (Show, Eq)

parseIcd10CmOrder :: String -> Maybe Icd10CmPcsOrder
parseIcd10CmOrder line = 
    let
        substring index count = take count $ drop index line
        parsedOrderNumber = read $ substring 0 5 :: Maybe Int
        parsedCode = substring 6 7 
        parsedHeader = substring 14 1 
        parsedIsHeader 
            | parsedHeader == "0" = False
            | otherwise = True
        parsedShortDescription = substring 16 60 
        parsedLongDescription = substring 77 (length line - 77) 
        makeIcd justOrderNumber = Icd10CmPcsOrder 
            { orderNumber = justOrderNumber
            , code = pack parsedCode
            , isHeader = parsedIsHeader
            , shortDescription = pack parsedShortDescription
            , longDescription = pack parsedLongDescription
            }
    in
    fmap makeIcd parsedOrderNumber
