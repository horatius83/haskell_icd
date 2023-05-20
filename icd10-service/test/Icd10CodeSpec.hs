module Icd10CodeSpec (spec) where 

import Test.Hspec (describe, context, it, shouldBe, Spec)
import Icd10Codes (parseIcd10CmOrder, parseIcd10CmOrders, Icd10CmPcsOrder(..))
import Data.Text (pack)

spec :: Spec
spec = do
    describe "parseIcd10CmOrderSpec" $ do
        context "when the line is valid and the header value is 0" $ do
            it "Should parse correctly with isHeader set to false" $ do
                parseIcd10CmOrder "00001 A00     0 Cholera                                                      Cholera" 
                    `shouldBe` Just Icd10CmPcsOrder 
                        { orderNumber = 1
                        , code = pack "A00"
                        , isHeader = False
                        , shortDescription = pack "Cholera"
                        , longDescription = pack "Cholera"
                        }
        context "when the line is valid and the header value is 1" $ do
            it "Should parse correctly with isHeader set to true" $ do
                parseIcd10CmOrder "96795 U099    1 Post COVID-19 condition, unspecified                         Post COVID-19 condition, unspecified"
                    `shouldBe` Just Icd10CmPcsOrder  
                        { orderNumber = 96795
                        , code = pack "U099"
                        , isHeader = True
                        , shortDescription = pack "Post COVID-19 condition, unspecified"
                        , longDescription = pack "Post COVID-19 condition, unspecified"
                        }
        context "when the line is not valid" $ do
            it "Should return nothing" $ do
                parseIcd10CmOrder "" `shouldBe` Nothing
    describe "parseIcd10CmOrders" $ do
        context "when all the lines are valid" $ do
            it "Should return all of the parsed lines" $ do
                parseIcd10CmOrders (lines "00001 A00     0 Cholera                                                      Cholera\n00002 A000    1 Cholera due to Vibrio cholerae 01, biovar cholerae           Cholera due to Vibrio cholerae 01, biovar cholerae")
                    `shouldBe` Right 
                        [ Icd10CmPcsOrder 
                            { orderNumber = 1
                            , code = pack "A00"
                            , isHeader = False
                            , shortDescription = pack "Cholera"
                            , longDescription = pack "Cholera"
                            }
                        , Icd10CmPcsOrder 
                            { orderNumber = 2
                            , code = pack "A000"
                            , isHeader = True
                            , shortDescription = pack "Cholera due to Vibrio cholerae 01, biovar cholerae"
                            , longDescription = pack "Cholera due to Vibrio cholerae 01, biovar cholerae"
                            }
                        ]
        context "when one of the lines is invalid" $ do
            it "Should return the text of the line that was invalid" $ do
                parseIcd10CmOrders (lines "00001 A00     0 Cholera                                                      Cholera\nTest test test")
                    `shouldBe` Left "Error parsing: Test test test"