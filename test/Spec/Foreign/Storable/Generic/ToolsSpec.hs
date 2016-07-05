module Foreign.Storable.Generic.ToolsSpec where

--Tested modules
import Foreign.Storable.Generic.Tools

import Test.Hspec
import Test.QuickCheck

spec :: Spec 
spec = do 
    describe "getFilling" $ do
        it "getFilling [(2,2),(4,4),(1,1),(2,2)] == [Size 2, Padding 2, Size 4, Size 1, Padding 1, Size 2]" $ do
            getFilling [(2,2),(4,4),(1,1),(2,2)] `shouldBe` [Size 2, Padding 2, Size 4, Size 1, Padding 1, Size 2]
        it "getFilling [] = []" $ do
            getFilling [] `shouldBe` []
    describe "calcSize" $ do
        it "calcSize [] = 0" $ do
            calcSize [] `shouldBe` 0
        it "equal to sum of padding" $ do
            let summer (Padding a) = a
                summer (Size    a) = a
            property $ do
                ls <- generate $ listOf $ suchThat arbitrary (\(a,b) -> a > 0 && b > 0)
                calcSize ls `shouldBe` (sum $ map summer $ getFilling ls)
    describe "calcOffsets" $ do
        it "calcOffsets [(2,2),(4,4),(1,1),(2,2)] == [0,4,8,10]" $ do
            calcOffsets [(2,2),(4,4),(1,1),(2,2)] `shouldBe` [0,4,8,10]
        it "calcOffsets [] = []" $ do
            calcOffsets [] `shouldBe` []
