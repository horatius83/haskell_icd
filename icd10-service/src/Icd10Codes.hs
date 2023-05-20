module Icd10COdes (
    Icd10CmPcsOrder
) where
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
{ OrderNumber :: Int
, Code :: String
, IsHeader :: Boolean
, ShortDescription :: String
, LongDescription :: String
}

parseIcd10CmOrder :: String -> Icd10CmPcsOrder
parseIcd10CmOrder line = Icd10CmPcsOrder