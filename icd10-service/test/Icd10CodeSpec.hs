module Icd10CodeSpec
  ( spec
  ) where

import           Data.Text  (pack)
import           Icd10Codes (Icd10CmPcsOrder (..), parseIcd10CmOrder,
                             parseIcd10CmOrders)
import           Test.Hspec (Spec, context, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "parseIcd10CmOrderSpec" $ do
    context "when the line is valid and the header value is 0" $ do
      it "Should parse correctly with isHeader set to false" $ do
        let text =
              "00001 A00     0 Cholera                                                      Cholera"
            expectedResult =
              Just $
              Icd10CmPcsOrder
                1
                (pack "A00")
                False
                (pack "Cholera")
                (pack "Cholera")
        parseIcd10CmOrder text `shouldBe` expectedResult
    context "when the line is valid and the header value is 1" $ do
      it "Should parse correctly with isHeader set to true" $ do
        let text =
              "96795 U099    1 Post COVID-19 condition, unspecified                         Post COVID-19 condition, unspecified"
            expectedResult =
              Just $
              Icd10CmPcsOrder
                96795
                (pack "U099")
                True
                (pack "Post COVID-19 condition, unspecified")
                (pack "Post COVID-19 condition, unspecified")
        parseIcd10CmOrder text `shouldBe` expectedResult
    context "when the line is not valid" $ do
      it "Should return nothing" $ do parseIcd10CmOrder "" `shouldBe` Nothing
  describe "parseIcd10CmOrders" $ do
    context "when all the lines are valid" $ do
      it "Should return all of the parsed lines" $ do
        let linesOfText =
              lines
                "00001 A00     0 Cholera                                                      Cholera\n00002 A000    1 Cholera due to Vibrio cholerae 01, biovar cholerae           Cholera due to Vibrio cholerae 01, biovar cholerae"
            expectedResults =
              [ Icd10CmPcsOrder
                  1
                  (pack "A00")
                  False
                  (pack "Cholera")
                  (pack "Cholera")
              , Icd10CmPcsOrder
                  2
                  (pack "A000")
                  True
                  (pack "Cholera due to Vibrio cholerae 01, biovar cholerae")
                  (pack "Cholera due to Vibrio cholerae 01, biovar cholerae")
              ]
        parseIcd10CmOrders linesOfText `shouldBe` (Right expectedResults)
    context "when one of the lines is invalid" $ do
      it "Should return the text of the line that was invalid" $ do
        parseIcd10CmOrders
          (lines
             "00001 A00     0 Cholera                                                      Cholera\nTest test test") `shouldBe`
          Left "Error parsing: Test test test"
