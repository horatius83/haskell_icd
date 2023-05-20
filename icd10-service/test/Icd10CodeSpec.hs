module Icd10CodeSpec (spec) where 

import Test.Hspec (hspec, describe, context, it, shouldBe, Spec)
import Icd10Codes (parseIcd10CmOrder, Icd10CmPcsOrder(..))
import Data.Text (pack)

spec :: Spec
spec = do
    describe "parseIcd10CmOrderSpec tests" $ do
        context "when the line is valid" $ do
            it "Should parse correctly" $ do
                parseIcd10CmOrder "00001 A00     0 Cholera                                                      Cholera" 
                    `shouldBe` Just Icd10CmPcsOrder 
                        { orderNumber = 1
                        , code = pack "A00"
                        , isHeader = False
                        , shortDescription = pack "Cholera"
                        , longDescription = pack "Cholera"
                        }