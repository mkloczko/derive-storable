{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE DeriveGeneric       #-}
module GenericTypeSpec where


-- Test tools
import Test.Hspec
import Test.QuickCheck

-- Tested modules
import GenericType 

-- Additional data
import Foreign.Storable.Generic
import Foreign.Ptr (Ptr)
import Foreign.Marshal.Alloc (malloc)
import Data.Int
import GHC.Generics
import Foreign.Storable.Generic.Internal
data TestData = TestData Int Int64 Int8 Int8
    deriving (Show, Generic, Eq)

instance GStorable TestData
instance Arbitrary TestData where
    arbitrary = TestData <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

spec :: Spec
spec = do 
    describe "instance (GStorable' f, GStorable' g) => GStorable (:*: f g p)" $ do
        it "glistSizeOf' @((:*:) f g p) == glistSizeOf' @(f p) ++ glistSizeOf' @(g p) " $ do
        -- Construct the tree using typeProduct
            property (\gen_type1 gen_type2 ->
                sequence_ [listOfSizes gen_type1 ++ listOfSizes gen_type2 `shouldBe` (listOfSizes $ typeProduct gen_type1 gen_type2)
                          ,listOfSizes gen_type2 ++ listOfSizes gen_type1 `shouldBe` (listOfSizes $ typeProduct gen_type2 gen_type1)
                          ])
        it "glistAlignment' @((:*:) f g p) == glistAlignment' @(f p) ++ glistAlignment' @(g p)" $ do
            property (\gen_type1 gen_type2 ->
                sequence_ [listOfAlignments gen_type1 ++ listOfAlignments gen_type2 `shouldBe` (listOfAlignments $ typeProduct gen_type1 gen_type2)
                          ,listOfAlignments gen_type2 ++ listOfAlignments gen_type1 `shouldBe` (listOfAlignments $ typeProduct gen_type2 gen_type1)
                          ])
        it "gpeekByteOff' works for both the test type and it's representation at :*: level" $ do
            property (\(gstor :: TestData)-> do
                -- The generic representation of TestData, without the M1 M1 constructors.
                let gen_rep  = unM1 $ unM1 $ from gstor
                -- Memory for the test data.
                ptr <- malloc :: IO (Ptr TestData)
                -- First peek of the raw memory.
                gen_rep_peeked  <- gpeekByteOff ptr 0
                gstor_peeked    <- gpeekByteOff ptr 0 :: IO TestData
                
                -- Save the gstor value in the pointer.
                -- Assumes that gpokeByteOff works.
                gpokeByteOff ptr 0 gstor
                -- Second peek to the modified memory
                gen_rep_peeked2  <- gpeekByteOff ptr 0 
                gstor_peeked2    <- gpeekByteOff ptr 0 :: IO TestData
                -- Get the values from generic reps back.
                let back_to_life  = to $ M1 $ M1 gen_rep_peeked  :: TestData
                    back_to_life2 = to $ M1 $ M1 gen_rep_peeked2 :: TestData                 
                -- Compare:
                sequence_ [back_to_life  `shouldBe`  gstor_peeked
                          ,back_to_life2 `shouldBe`  gstor_peeked2
                          ]
                )
        it "gpokeByteOff' works for both the test type and it's representation at :*: level" $ do
            property (\(gstor :: TestData)-> do
                -- The generic representation of TestData, without the M1 M1 constructors.
                let gen_rep  = unM1 $ unM1 $ from gstor
                -- Memory for the test data.
                ptr1 <- malloc :: IO (Ptr TestData) 
                ptr2 <- malloc :: IO (Ptr TestData)
                -- Poke the memory
                gpokeByteOff ptr1 0 gstor
                gpokeByteOff ptr2 0 gen_rep
                -- Read the memory
                -- Assumes that gpeekByteOff works
                ptr1_peeked <- gpeekByteOff ptr1 0 :: IO TestData
                ptr2_peeked <- gpeekByteOff ptr2 0 :: IO TestData
                
                ptr1_peeked `shouldBe` ptr2_peeked
                )
        it "gsizeOf a == gsizeOf (unM1 $ unM1 $ from a)" $ do
            property (\(gstor :: TestData) -> do
                let gen_rep = unM1 $ unM1 $ from gstor
                gsizeOf gstor `shouldBe` gsizeOf gen_rep
                )
        it "galignment a == galignment (unM1 $ unM1 $ from a)" $ do
            property (\(gstor :: TestData) -> do
                let gen_rep = unM1 $ unM1 $ from gstor
                galignment gstor `shouldBe` galignment gen_rep
                )
    describe "listOfSizes" $ do
        it "listOfSizes val == glistSizeOf' ((Full | Part | Product) rep)" $ do
            property (\gen_type -> do
                let test1 (Full    rep) = glistSizeOf' rep
                    test1 (Part    rep) = glistSizeOf' rep
                    test1 (Product rep) = glistSizeOf' rep
                listOfSizes gen_type `shouldBe` test1 gen_type
                )
    describe "listOfAlignments" $ do
        it "listOfAlignments val == glistAlignment' ((Full | Part | Product) rep)" $ do
            property (\gen_type -> do
                let test1 (Full    rep) = glistAlignment' rep
                    test1 (Part    rep) = glistAlignment' rep
                    test1 (Product rep) = glistAlignment' rep
                listOfAlignments gen_type `shouldBe` test1 gen_type
                )
